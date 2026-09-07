%% @doc Track source-owned documentation and reconcile removed source pages.
%%
%% The tracking table deliberately lives outside the resource props. This makes
%% imports auditable and lets a later live-site migration distinguish source
%% documentation from editorial content without relying on categories alone.
%% Subject keywords on source-managed documentation are authoritative: every
%% import replaces those `subject` edges with the `zotonic_keywords` metadata
%% from the compiled module or callback documentation.
%% @end

-module(zotonicwww2_doc_import).

-export([
    install/1,
    sync/3,
    keyword_coverage/1,
    migrate_legacy/1,
    migration_status/1,
    register_delivery/3,
    unregister_delivery/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(TABLE, zotonicwww2_doc_import).
-define(TOPIC_PREFIX, <<"zotonic_topic_">>).


%% @doc Install the import tracking table. Safe to call on every schema update.
-spec install(z:context()) -> ok.
install(Context) ->
    ok = install_import_table(Context),
    install_delivery_table(Context).

install_import_table(Context) ->
    case z_db:table_exists(?TABLE, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table zotonicwww2_doc_import (
                    rsc_id integer not null,
                    source_key character varying(300) not null,
                    source_kind character varying(40) not null,
                    source_path character varying(500),
                    source_hash character varying(64) not null,
                    generation character varying(40) not null,
                    git_commit character varying(64) not null,
                    status character varying(20) not null default 'current',
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    constraint zotonicwww2_doc_import_pkey primary key (rsc_id),
                    constraint zotonicwww2_doc_import_source_key_key unique (source_key),
                    constraint fk_zotonicwww2_doc_import_rsc_id foreign key (rsc_id)
                        references rsc(id) on update cascade on delete cascade
                )",
                Context),
            [] = z_db:q("
                create index zotonicwww2_doc_import_generation_key
                on zotonicwww2_doc_import(generation)",
                Context),
            [] = z_db:q("
                create index zotonicwww2_doc_import_status_key
                on zotonicwww2_doc_import(status)",
                Context),
            z_db:flush(Context)
    end.

install_delivery_table(Context) ->
    case z_db:table_exists(zotonicwww2_webhook_delivery, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table zotonicwww2_webhook_delivery (
                    delivery_id character varying(100) not null,
                    git_commit character varying(64) not null,
                    created timestamp with time zone not null default now(),
                    constraint zotonicwww2_webhook_delivery_pkey primary key (delivery_id)
                )",
                Context),
            [] = z_db:q("
                create index zotonicwww2_webhook_delivery_created_key
                on zotonicwww2_webhook_delivery(created)",
                Context),
            z_db:flush(Context)
    end.


%% @doc Register a GitHub delivery id. The primary key provides replay
%% protection even when two identical deliveries arrive concurrently.
-spec register_delivery(binary(), binary(), z:context()) -> new | duplicate.
register_delivery(DeliveryId, Commit, Context) ->
    _ = z_db:q("
        delete from zotonicwww2_webhook_delivery
        where created < now() - interval '30 days'",
        Context),
    case z_db:q1("
        insert into zotonicwww2_webhook_delivery (delivery_id, git_commit)
        values ($1, $2)
        on conflict (delivery_id) do nothing
        returning delivery_id",
        [ DeliveryId, Commit ],
        Context)
    of
        DeliveryId -> new;
        undefined -> duplicate
    end.


%% @doc Forget a delivery when it could not be queued, so GitHub can retry it.
-spec unregister_delivery(binary(), z:context()) -> ok.
unregister_delivery(DeliveryId, Context) ->
    _ = z_db:q(
        "delete from zotonicwww2_webhook_delivery where delivery_id = $1",
        [ DeliveryId ],
        Context),
    ok.


%% @doc Synchronize a complete source manifest and deprecate tracked pages which
%% are no longer present. Reconciliation only starts after every manifest entry
%% has been stored successfully.
-spec sync([map()], binary(), z:context()) -> {ok, map()} | {error, term()}.
sync(Entries, Commit, Context0) when is_list(Entries), is_binary(Commit) ->
    Context = z_acl:sudo(Context0),
    case validate_manifest_observes(Entries) of
        ok ->
            sync_with_keywords(Entries, Commit, Context);
        {error, _} = Error ->
            Error
    end.

sync_with_keywords(Entries, Commit, Context) ->
    case resolve_manifest_keywords(Entries, Context) of
        {ok, KeywordIds} ->
            Generation = z_ids:id(20),
            Report0 = #{created => 0, updated => 0, unchanged => 0, deprecated => 0},
            case sync_entries(Entries, Commit, Generation, KeywordIds, Report0, Context) of
                {ok, Report1} ->
                    case reconcile(Generation, Context) of
                        {ok, Deprecated} ->
                            {ok, Report1#{
                                deprecated => Deprecated,
                                total => length(Entries),
                                generation => Generation,
                                commit => Commit
                            }};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

sync_entries([], _Commit, _Generation, _KeywordIds, Report, _Context) ->
    {ok, Report};
sync_entries([Entry | Rest], Commit, Generation, KeywordIds, Report, Context) ->
    case sync_entry(Entry, Commit, Generation, KeywordIds, Context) of
        {ok, Result} ->
            sync_entries(
                Rest,
                Commit,
                Generation,
                KeywordIds,
                maps:update_with(Result, fun(N) -> N + 1 end, 1, Report),
                Context);
        {error, _} = Error ->
            Error
    end.

sync_entry(Entry, Commit, Generation, KeywordIds, Context) ->
    #{
        category := Category,
        kind := Kind,
        name := Name,
        title := Title,
        body := Body,
        source_path := SourcePath
    } = Entry,
    EntryContext = entry_context(Kind, Context),
    SourceKey = source_key(Name),
    SourceHash = source_hash(Entry),
    Tracking = tracking(SourceKey, EntryContext),
    ExistingId = m_rsc:rid(Name, EntryContext),
    Result = change_kind(Tracking, ExistingId, SourceHash),
    SourceUrl = source_url(SourcePath),
    Props0 = maps:merge(#{
        <<"name">> => Name,
        <<"title">> => Title,
        <<"body">> => Body,
        <<"is_published">> => true,
        <<"category_id">> => Category,
        <<"content_group_id">> => imported_group(EntryContext),
        <<"doc_status">> => <<"current">>,
        <<"github_url">> => SourceUrl,
        <<"doc_source_kind">> => atom_to_binary(Kind),
        <<"doc_source_path">> => SourcePath,
        <<"doc_source_commit">> => Commit,
        <<"doc_source_hash">> => SourceHash
    }, maps:get(props, Entry, #{})),
    Props = z_sanitize:escape_props_check(Props0, EntryContext),
    case store_resource(Result, ExistingId, Props, EntryContext) of
        {ok, RscId} ->
            case replace_import_edges(RscId, Entry, KeywordIds, EntryContext) of
                ok ->
                    ok = track(
                        RscId,
                        SourceKey,
                        Kind,
                        SourcePath,
                        SourceHash,
                        Generation,
                        Commit,
                        EntryContext),
                    {ok, Result};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

entry_context(release, Context) -> z_context:set_tz(<<"UTC">>, Context);
entry_context(_Kind, Context) -> Context.

change_kind({ok, #{ <<"source_hash">> := Hash, <<"status">> := <<"current">> }}, RscId, Hash)
    when is_integer(RscId) ->
    unchanged;
change_kind({ok, _}, RscId, _Hash) when is_integer(RscId) ->
    updated;
change_kind({error, enoent}, RscId, _Hash) when is_integer(RscId) ->
    updated;
change_kind(_Tracking, undefined, _Hash) ->
    created.

store_resource(created, _RscId, Props, Context) ->
    m_rsc:insert(Props, Context);
store_resource(_Result, RscId, Props, Context) when is_integer(RscId) ->
    m_rsc:update(RscId, Props, Context).

replace_import_edges(RscId, Entry, KeywordIds, Context) ->
    case replace_module_edge(RscId, maps:get(module, Entry, undefined), Context) of
        ok ->
            case replace_observes_edges(RscId, Entry, Context) of
                ok -> replace_subject_edges(RscId, Entry, KeywordIds, Context);
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

replace_module_edge(_RscId, undefined, _Context) ->
    ok;
replace_module_edge(RscId, Module, Context) when is_binary(Module) ->
    ModName = module_page_name(Module),
    case m_rsc:rid(ModName, Context) of
        undefined -> {error, {unknown_module, ModName}};
        ModId -> m_edge:replace(RscId, in_module, [ ModId ], Context)
    end.

replace_observes_edges(RscId, #{kind := module, observes := Notifications}, Context) ->
    replace_observes_edges_1(RscId, Notifications, [], Context);
replace_observes_edges(_RscId, _Entry, _Context) ->
    ok.

replace_observes_edges_1(RscId, [], NotificationIds, Context) ->
    m_edge:replace(RscId, observes, lists:reverse(NotificationIds), Context);
replace_observes_edges_1(RscId, [Notification | Rest], NotificationIds, Context) ->
    Name = notification_page_name(Notification),
    case m_rsc:rid(Name, Context) of
        undefined -> {error, {unknown_notification, Notification}};
        NotificationId ->
            replace_observes_edges_1(RscId, Rest, [NotificationId | NotificationIds], Context)
    end.

replace_subject_edges(RscId, #{keywords := KeywordSlugs}, KeywordIds, Context) ->
    SubjectIds = [ maps:get(Slug, KeywordIds) || Slug <- KeywordSlugs ],
    m_edge:replace(RscId, subject, SubjectIds, Context);
replace_subject_edges(_RscId, _Entry, _KeywordIds, _Context) ->
    ok.

%% Validate all observer targets against the same complete manifest before
%% changing resources. This catches an undocumented notification without
%% leaving a partially updated relationship graph.
validate_manifest_observes(Entries) ->
    NotificationNames = sets:from_list([
        Name
        || #{kind := notification, name := Name} <- Entries
    ]),
    validate_manifest_observes(Entries, NotificationNames).

validate_manifest_observes([], _NotificationNames) ->
    ok;
validate_manifest_observes([
        #{kind := module, name := ModuleName, observes := Notifications} | Rest
    ], NotificationNames) when is_list(Notifications) ->
    case lists:dropwhile(
        fun(Notification) ->
            sets:is_element(notification_page_name(Notification), NotificationNames)
        end,
        Notifications)
    of
        [] -> validate_manifest_observes(Rest, NotificationNames);
        [Unknown | _] -> {error, {unknown_observed_notification, ModuleName, Unknown}}
    end;
validate_manifest_observes([#{kind := module, name := ModuleName} | _], _NotificationNames) ->
    {error, {missing_module_observes, ModuleName}};
validate_manifest_observes([_Entry | Rest], NotificationNames) ->
    validate_manifest_observes(Rest, NotificationNames).

%% Resolve the complete manifest before changing any resource. This makes a
%% missing or misspelled slug a deterministic import error instead of silently
%% producing incomplete subject navigation.
resolve_manifest_keywords(Entries, Context) ->
    case manifest_keyword_slugs(Entries) of
        {ok, Slugs} -> resolve_keyword_ids(Slugs, #{}, Context);
        {error, _} = Error -> Error
    end.

manifest_keyword_slugs(Entries) ->
    manifest_keyword_slugs(Entries, []).

manifest_keyword_slugs([], Acc) ->
    {ok, lists:usort(Acc)};
manifest_keyword_slugs([#{keywords := Keywords, name := Name} | Rest], Acc)
    when is_list(Keywords) ->
    case lists:all(fun is_keyword_slug/1, Keywords) of
        true -> manifest_keyword_slugs(Rest, Keywords ++ Acc);
        false -> {error, {invalid_manifest_keywords, Name, Keywords}}
    end;
manifest_keyword_slugs([#{name := _Name} | Rest], Acc) ->
    manifest_keyword_slugs(Rest, Acc);
manifest_keyword_slugs([Entry | _Rest], _Acc) ->
    {error, {invalid_manifest_entry, Entry}}.

is_keyword_slug(Slug) ->
    is_binary(Slug) andalso Slug =/= <<>>.

resolve_keyword_ids([], Acc, _Context) ->
    {ok, Acc};
resolve_keyword_ids([Slug | Rest], Acc, Context) ->
    Name = keyword_resource_name(Slug),
    case m_rsc:rid(Name, Context) of
        undefined ->
            {error, {unknown_subject_keyword, Slug}};
        Id ->
            case {
                m_rsc:is_a(Id, keyword, Context),
                m_rsc:p_no_acl(Id, <<"subject_topic_slug">>, Context)
            } of
                {true, Slug} -> resolve_keyword_ids(Rest, Acc#{Slug => Id}, Context);
                _ -> {error, {invalid_subject_keyword, Slug, Id}}
            end
    end.

keyword_resource_name(Slug) ->
    <<(?TOPIC_PREFIX)/binary, Slug/binary>>.

module_page_name(<<"zotonic_core">>) -> <<"doc_core">>;
module_page_name(Module) -> <<"doc_module_", Module/binary>>.

notification_page_name(Notification) ->
    <<"doc_notification_", Notification/binary>>.

tracking(SourceKey, Context) ->
    z_db:qmap_row("
        select rsc_id, source_hash, status
        from zotonicwww2_doc_import
        where source_key = $1",
        [ SourceKey ],
        Context).

track(RscId, SourceKey, Kind, SourcePath, SourceHash, Generation, Commit, Context) ->
    _ = z_db:q("
        insert into zotonicwww2_doc_import
            (rsc_id, source_key, source_kind, source_path, source_hash,
             generation, git_commit, status)
        values ($1, $2, $3, $4, $5, $6, $7, 'current')
        on conflict (source_key) do update set
            rsc_id = excluded.rsc_id,
            source_kind = excluded.source_kind,
            source_path = excluded.source_path,
            source_hash = excluded.source_hash,
            generation = excluded.generation,
            git_commit = excluded.git_commit,
            status = 'current',
            modified = now()",
        [ RscId, SourceKey, atom_to_binary(Kind), SourcePath, SourceHash, Generation, Commit ],
        Context),
    ok.

reconcile(Generation, Context) ->
    case z_db:qmap("
        select rsc_id
        from zotonicwww2_doc_import
        where status = 'current' and generation <> $1",
        [ Generation ],
        Context)
    of
        {ok, Rows} -> reconcile_rows(Rows, 0, Context);
        {error, _} = Error -> Error
    end.

reconcile_rows([], Count, _Context) ->
    {ok, Count};
reconcile_rows([#{ <<"rsc_id">> := RscId } | Rest], Count, Context) ->
    Props = #{
        <<"is_published">> => false,
        <<"content_group_id">> => deprecated_group(Context),
        <<"doc_status">> => <<"deprecated">>
    },
    case m_rsc:update(RscId, Props, Context) of
        {ok, _} ->
            _ = z_db:q("
                update zotonicwww2_doc_import
                set status = 'deprecated', modified = now()
                where rsc_id = $1",
                [ RscId ],
                Context),
            reconcile_rows(Rest, Count + 1, Context);
        {error, _} = Error ->
            Error
    end.


%% @doc Report subject-keyword coverage for every currently imported
%% documentation category. A resource is covered when it has at least one
%% `subject` edge to a resource in the keyword category tree.
-spec keyword_coverage(z:context()) -> {ok, map()} | {error, term()}.
keyword_coverage(Context0) ->
    Context = z_acl:sudo(Context0),
    SubjectId = m_rsc:rid(subject, Context),
    {KeywordFrom, KeywordTo} = m_category:get_range_by_name(keyword, Context),
    case z_db:qmap("
        select
            category.id as category_id,
            category.name as category,
            count(distinct doc.id) as total,
            count(distinct doc.id) filter (where keyword.id is not null) as covered,
            count(keyword.id) as keyword_count
        from zotonicwww2_doc_import doc_import
        join rsc doc
          on doc.id = doc_import.rsc_id
        join rsc category
          on category.id = doc.category_id
        left join edge
          on edge.subject_id = doc.id
         and edge.predicate_id = $1
        left join rsc keyword
          on keyword.id = edge.object_id
         and keyword.pivot_category_nr >= $2
         and keyword.pivot_category_nr <= $3
        where doc_import.status = 'current'
        group by category.id, category.name
        order by category.name",
        [ SubjectId, KeywordFrom, KeywordTo ],
        Context)
    of
        {ok, Rows} ->
            Coverage = [ keyword_coverage_row(Row) || Row <- Rows ],
            ?LOG_INFO(#{
                in => zotonicwww2,
                text => <<"Documentation keyword coverage">>,
                result => ok,
                keyword_coverage => Coverage
            }),
            {ok, #{keyword_coverage => Coverage}};
        {error, _} = Error ->
            Error
    end.

keyword_coverage_row(#{
    <<"category_id">> := CategoryId,
    <<"category">> := Category,
    <<"total">> := Total,
    <<"covered">> := Covered,
    <<"keyword_count">> := KeywordCount
}) ->
    #{
        category_id => CategoryId,
        category => Category,
        total => Total,
        covered => Covered,
        missing => Total - Covered,
        coverage_percent => coverage_percent(Covered, Total),
        keyword_count => KeywordCount
    }.

coverage_percent(_Covered, 0) -> 0;
coverage_percent(Covered, Total) -> (Covered * 100 + Total div 2) div Total.


%% @doc Adopt source-looking legacy resources which predate the tracking table.
%% This is intentionally explicit; it is not run during a schema upgrade.
-spec migrate_legacy(z:context()) -> {ok, map()} | {error, term()}.
migrate_legacy(Context0) ->
    Context = z_acl:sudo(Context0),
    case latest_generation(Context) of
        undefined ->
            {error, import_required};
        Generation ->
            {ok, Candidates} = legacy_candidates(Context),
            migrate_legacy(Candidates, Generation, 0, Context)
    end.

migrate_legacy([], _Generation, Count, _Context) ->
    {ok, #{deprecated => Count}};
migrate_legacy([#{ <<"id">> := RscId, <<"name">> := Name } | Rest], Generation, Count, Context) ->
    SourceKey = source_key(Name),
    case tracking(SourceKey, Context) of
        {ok, _} ->
            migrate_legacy(Rest, Generation, Count, Context);
        {error, enoent} ->
            Kind = legacy_kind(Name),
            ok = track(RscId, SourceKey, Kind, <<"legacy">>, <<"legacy">>, <<"legacy">>, <<"legacy">>, Context),
            case reconcile_rows([#{ <<"rsc_id">> => RscId }], 0, Context) of
                {ok, 1} -> migrate_legacy(Rest, Generation, Count + 1, Context);
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec migration_status(z:context()) -> map().
migration_status(Context) ->
    Tracked = z_db:q1("select count(*) from zotonicwww2_doc_import", Context),
    Deprecated = z_db:q1("select count(*) from zotonicwww2_doc_import where status = 'deprecated'", Context),
    {ok, Candidates} = legacy_candidates(Context),
    #{
        tracked => Tracked,
        deprecated => Deprecated,
        legacy_candidates => length(Candidates)
    }.

legacy_candidates(Context) ->
    z_db:qmap("
        select id, name
        from rsc
        where name is not null
          and (
              name like 'doc_module_%'
              or name like 'doc_controller_controller_%'
              or name like 'doc_model_model_%'
              or name like 'doc_template_filter_filter_%'
              or name like 'doc_template_scomp_scomp_%'
              or name like 'doc_template_action_action_%'
              or name like 'doc_template_validator_validator_%'
              or name like 'doc_template_tag_tag_%'
              or name like 'doc_dispatch_dispatch_%'
              or name like 'doc_notification_%'
              or name like 'doc_releasenotes_rel_%'
          )
          and not exists (
              select 1 from zotonicwww2_doc_import di where di.rsc_id = rsc.id
          )",
        Context).

latest_generation(Context) ->
    z_db:q1("
        select generation
        from zotonicwww2_doc_import
        where status = 'current'
        order by modified desc
        limit 1",
        Context).

legacy_kind(Name) ->
    case Name of
        <<"doc_module_", _/binary>> -> module;
        <<"doc_controller_", _/binary>> -> controller;
        <<"doc_model_", _/binary>> -> model;
        <<"doc_template_filter_", _/binary>> -> filter;
        <<"doc_template_scomp_", _/binary>> -> scomp;
        <<"doc_template_action_", _/binary>> -> action;
        <<"doc_template_validator_", _/binary>> -> validator;
        <<"doc_template_tag_", _/binary>> -> template_tag;
        <<"doc_dispatch_", _/binary>> -> dispatch;
        <<"doc_notification_", _/binary>> -> notification;
        <<"doc_releasenotes_", _/binary>> -> release
    end.

source_key(Name) ->
    <<"zotonic:", Name/binary>>.

source_hash(Entry) ->
    z_url:hex_encode_lc(crypto:hash(sha256, term_to_binary(maps:without([source_url], Entry)))).

source_url(SourcePath) ->
    <<"https://github.com/zotonic/zotonic/blob/master/", SourcePath/binary>>.

imported_group(Context) ->
    m_rsc:rid(content_group_imported_docs, Context).

deprecated_group(Context) ->
    m_rsc:rid(content_group_deprecated_docs, Context).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

source_url_uses_master_test() ->
    ?assertEqual(
        <<"https://github.com/zotonic/zotonic/blob/master/apps/zotonic_core/src/zotonic.erl">>,
        source_url(<<"apps/zotonic_core/src/zotonic.erl">>)).

manifest_keyword_slugs_test() ->
    Entries = [
        #{
            name => <<"doc_module_mod_media">>,
            keywords => [<<"media_management">>, <<"upload">>]
        },
        #{
            name => <<"doc_notification_media_upload">>,
            keywords => [<<"upload">>, <<"notification">>]
        },
        #{name => <<"doc_releasenotes_rel_1.0.0">>}
    ],
    ?assertEqual(
        {ok, [<<"media_management">>, <<"notification">>, <<"upload">>]},
        manifest_keyword_slugs(Entries)).

invalid_manifest_keywords_test() ->
    ?assertEqual(
        {error, {
            invalid_manifest_keywords,
            <<"doc_module_mod_media">>,
            [<<"media_management">>, invalid]
        }},
        manifest_keyword_slugs([
            #{
                name => <<"doc_module_mod_media">>,
                keywords => [<<"media_management">>, invalid]
            }
        ])).

valid_manifest_observes_test() ->
    ?assertEqual(
        ok,
        validate_manifest_observes([
            #{
                kind => notification,
                name => <<"doc_notification_media_upload">>
            },
            #{
                kind => module,
                name => <<"doc_module_mod_media">>,
                observes => [<<"media_upload">>]
            }
        ])).

unknown_manifest_observes_test() ->
    ?assertEqual(
        {error, {
            unknown_observed_notification,
            <<"doc_module_mod_media">>,
            <<"missing_notification">>
        }},
        validate_manifest_observes([
            #{
                kind => notification,
                name => <<"doc_notification_media_upload">>
            },
            #{
                kind => module,
                name => <<"doc_module_mod_media">>,
                observes => [<<"missing_notification">>]
            }
        ])).

keyword_resource_name_test() ->
    ?assertEqual(
        <<"zotonic_topic_search_and_discovery">>,
        keyword_resource_name(<<"search_and_discovery">>)).

keyword_coverage_row_test() ->
    ?assertEqual(
        #{
            category_id => 42,
            category => <<"notification">>,
            total => 12,
            covered => 7,
            missing => 5,
            coverage_percent => 58,
            keyword_count => 19
        },
        keyword_coverage_row(#{
            <<"category_id">> => 42,
            <<"category">> => <<"notification">>,
            <<"total">> => 12,
            <<"covered">> => 7,
            <<"keyword_count">> => 19
        })).

-endif.
