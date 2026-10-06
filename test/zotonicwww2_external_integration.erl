%% Run explicitly against a development site. All test data is rolled back.

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(zotonicwww2_external_integration).
-moduledoc("
Integration checks for external documentation on a running development site.

Run `run/1` explicitly with a development site context. Exercises external
module imports, dispatch rules, observers, configuration, admin templates,
and deprecation. All fixture data is created in a transaction that is rolled
back, and the site's resource cache is flushed afterwards.
").
-export([run/1]).

run(Context) ->
    C = z_acl:sudo(Context),
    {error, eacces} = m_zotonicwww2_external:m_get([<<"list">>], undefined, z_acl:anondo(Context)),
    Result = z_db:transaction(fun(Tx) ->
        ok = dispatch_checks(Tx),
        Suffix = z_ids:id(12),
        A = <<"test_external_a_", Suffix/binary>>,
        B = <<"test_external_b_", Suffix/binary>>,
        Child = <<"test_external_child_", Suffix/binary>>,
        Config = [#{<<"module">> => <<"site">>, <<"key">> => <<"example">>,
            <<"has_default">> => true, <<"default">> => <<"false">>,
            <<"type">> => <<"boolean">>, <<"description">> => <<"Literal <config> & text">>}],
        Entry = #{category => module, kind => external, name => A, title => <<"External test">>,
            body => <<"<p>First version</p><script>alert(1)</script>">>,
            keywords => [], source_path => <<"src/mod_test.erl">>,
            source_url => <<"https://example.com/repo">>,
            props => #{<<"doc_module_config">> => Config, <<"is_external_module">> => true, <<"git_url">> => <<"https://example.com/repo">>}},
        ChildEntry = Entry#{name => Child, category => model, module => {page, A}},
        {ok, #{created := 2}} = zotonicwww2_doc_import:sync_external(<<"external_2147483646">>, [Entry, ChildEntry], <<"a">>, Tx),
        {ok, #{created := 1}} = zotonicwww2_doc_import:sync_external(<<"external_2147483645">>, [Entry#{name => B}], <<"b">>, Tx),
        IdA = m_rsc:rid(A, Tx),
        IdB = m_rsc:rid(B, Tx),
        IdChild = m_rsc:rid(Child, Tx),
        nomatch = binary:match(m_rsc:p(IdA, body, Tx), <<"<script">>),
        {ConfigHtml, _} = z_render:output(z_template:render(<<"_module_configuration.tpl">>, [{module_id, IdA}], Tx), Tx),
        ConfigBin = iolist_to_binary(ConfigHtml),
        true = binary:match(ConfigBin, <<"<code>false</code>">>) =/= nomatch,
        true = binary:match(ConfigBin, <<"Literal &lt;config&gt; &amp; text">>) =/= nomatch
            orelse error({unexpected_config_html, ConfigBin}),
        {ModulePage, _} = z_render:output(z_template:render_block(content_after, <<"page.module.tpl">>, [{id, IdA}], Tx), Tx),
        true = binary:match(iolist_to_binary(ModulePage), <<"module-configuration">>) =/= nomatch,
        {ok, _} = zotonicwww2_doc_import:sync_external(<<"external_2147483646">>,
            [Entry#{props => (maps:get(props, Entry))#{<<"doc_module_config">> => []}}, ChildEntry], <<"config-removed">>, Tx),
        [] = m_rsc:p(IdA, doc_module_config, Tx),
        [IdA] = m_edge:objects(IdChild, in_module, Tx),
        {ok, #{updated := 1, deprecated := 1}} = zotonicwww2_doc_import:sync_external(
            <<"external_2147483646">>, [Entry#{body => <<"<p>Updated</p>">>}], <<"c">>, Tx),
        false = m_rsc:p(IdChild, is_published, Tx),
        true = m_rsc:p(IdB, is_published, Tx),
        {ok, #{deprecated := 1}} = zotonicwww2_doc_import:sync_external(<<"external_2147483646">>, [], <<"d">>, Tx),
        false = m_rsc:p(IdA, is_published, Tx),
        true = m_rsc:p(IdB, is_published, Tx),
        {ok, #{updated := 1}} = zotonicwww2_doc_import:sync_external(<<"external_2147483646">>, [Entry], <<"e">>, Tx),
        true = m_rsc:p(IdA, is_published, Tx),
        {Page, _} = z_render:output(z_template:render_block(content, <<"page.documentation.tpl">>, [{id, IdA}], Tx), Tx),
        true = binary:match(iolist_to_binary(Page), <<"External module">>) =/= nomatch,
        RepoId = z_db:q1("insert into zotonicwww2_external (title, git_url, report)
            values ($1,$2,$3) returning id", [<<"External <test>">>, <<"https://example.com/repo">>,
            z_json:encode([#{<<"path">> => <<"broken.erl">>, <<"error">> => <<"<script>test</script>">>}])], Tx),
        {Admin, _} = z_render:output(z_template:render_block(content, <<"admin_external_modules.tpl">>, [], Tx), Tx),
        Html = iolist_to_binary(Admin),
        nomatch = binary:match(Html, <<"<form">>),
        true = binary:match(Html, <<"<table">>) =/= nomatch,
        true = binary:match(Html, <<"&lt;test&gt;">>) =/= nomatch,
        {ok, {Repos, []}} = m_zotonicwww2_external:m_get([<<"list">>], undefined, Tx),
        [Repo] = [R || #{<<"id">> := N} = R <- Repos, N =:= RepoId],
        {Report, _} = z_render:output(z_template:render(
            <<"_admin_external_module_report.tpl">>, [{repo, Repo}], Tx), Tx),
        true = binary:match(iolist_to_binary(Report), <<"&lt;script&gt;test&lt;/script&gt;">>) =/= nomatch,
        {Edit, _} = z_render:output(z_template:render(
            <<"_admin_external_module_form.tpl">>, [{repo, Repo}], Tx), Tx),
        true = binary:match(iolist_to_binary(Edit), <<"value=\"External &lt;test&gt;\"">>) =/= nomatch,
        {New, _} = z_render:output(z_template:render(
            <<"_admin_external_module_form.tpl">>, [], Tx), Tx),
        true = binary:match(iolist_to_binary(New), <<"value=\"1\" checked">>) =/= nomatch,
        DeprecateSource = <<"external_", (integer_to_binary(RepoId))/binary>>,
        DeprecateName = <<"test_deprecate_", Suffix/binary>>,
        DeprecateChild = <<DeprecateName/binary, "_dispatch">>,
        {ok, _} = zotonicwww2_doc_import:sync_external(DeprecateSource, [
            Entry#{name => DeprecateName},
            Entry#{name => DeprecateChild, category => dispatch, module => {page, DeprecateName}}
        ], <<"deprecate-test">>, Tx),
        {error, eacces} = m_zotonicwww2_external:deprecate(RepoId, z_acl:anondo(Tx)),
        true = m_rsc:p(DeprecateName, is_published, Tx),
        {ok, #{deprecated := 2}} = m_zotonicwww2_external:deprecate(RepoId, Tx),
        DeprecatedGroup = m_rsc:rid(content_group_deprecated_docs, Tx),
        lists:foreach(fun(Name) ->
            false = m_rsc:p(Name, is_published, Tx),
            DeprecatedGroup = m_rsc:p(Name, content_group_id, Tx),
            <<"deprecated">> = m_rsc:p(Name, doc_status, Tx)
        end, [DeprecateName, DeprecateChild]),
        true = m_rsc:p(IdB, is_published, Tx),
        {ok, #{<<"is_enabled">> := false, <<"status">> := <<"deprecated">>}} =
            m_zotonicwww2_external:get(RepoId, Tx),
        ok = zotonicwww2_external_import:task_run(RepoId, true, Tx),
        false = m_rsc:p(DeprecateName, is_published, Tx),
        {ok, #{deprecated := 0}} = m_zotonicwww2_external:deprecate(RepoId, Tx),
        {rollback, verified}
    end, C),
    z_depcache:flush(C),
    verified = Result,
    ok.


%% Exercise the actual external collector, then persist, update, and remove a
%% dispatch page. An unrelated application's file must not borrow this parent.
dispatch_checks(Context) ->
    Suffix = z_ids:id(12),
    Module = <<"mod_test_", Suffix/binary>>,
    Controller = <<"controller_test_", Suffix/binary>>,
    Root = <<"./apps/", Suffix/binary>>,
    Repo = #{<<"id">> => 2147483644, <<"title">> => <<"Dispatch test">>,
        <<"git_url">> => <<"https://example.com/repo">>, <<"website_url">> => <<>>, <<"hex_package">> => <<>>},
    Source = #{<<"kind">> => <<"erlang">>, <<"status">> => <<"parsed">>,
        <<"doc">> => <<"Test documentation">>, <<"keywords">> => []},
    Notification = <<"test_notification_", Suffix/binary>>,
    CustomNotification = <<"custom_notification_", Suffix/binary>>,
    {ok, NotificationId} = m_rsc:insert(#{
        <<"name">> => <<"doc_notification_", Notification/binary>>,
        <<"category_id">> => notification, <<"title">> => Notification,
        <<"is_published">> => true}, Context),
    ModuleRow = Source#{<<"observes">> => [Notification, CustomNotification], <<"module">> => Module, <<"path">> => <<Root/binary, "/src/", Module/binary, ".erl">>},
    ControllerRow = Source#{<<"module">> => Controller, <<"path">> => <<Root/binary, "/src/controllers/", Controller/binary, ".erl">>},
    Rule = #{<<"name">> => <<"test">>, <<"path">> => <<"/test/:id">>,
        <<"controller">> => Controller, <<"options">> => <<"[{template, \"<example>\"}]">>},
    DispatchRow = #{<<"kind">> => <<"dispatch">>, <<"status">> => <<"parsed">>,
        <<"path">> => <<Root/binary, "/priv/dispatch/dispatch">>, <<"rules">> => [Rule]},
    Orphan = DispatchRow#{<<"path">> => <<"./apps/undocumented/priv/dispatch/dispatch">>},
    {Entries, Report} = zotonicwww2_external_import:collect_entries([ModuleRow, ControllerRow, DispatchRow, Orphan], Repo, Context),
    3 = length(Entries),
    1 = length([R || #{<<"status">> := <<"skipped">>} = R <- Report]),
    [#{name := DispatchName, body := Body}] = [E || #{category := dispatch} = E <- Entries],
    true = binary:match(Body, <<"&lt;example&gt;">>) =/= nomatch,
    {ok, #{created := 3}} = zotonicwww2_doc_import:sync_external(<<"external_2147483644">>, Entries, <<"dispatch-first">>, Context),
    DispatchId = m_rsc:rid(DispatchName, Context),
    true = m_rsc:is_a(DispatchId, dispatch, Context),
    true = m_rsc:p(DispatchId, is_external_module, Context),
    [ParentId] = m_edge:objects(DispatchId, in_module, Context),
    Module = m_rsc:p(ParentId, title, Context),
    [NotificationId] = m_edge:objects(ParentId, observes, Context),
    2 = length(m_rsc:p(ParentId, doc_module_observers, Context)),
    {ObserversHtml, _} = z_render:output(z_template:render(<<"_module_observes.tpl">>, [{module_id, ParentId}], Context), Context),
    ObserversBin = iolist_to_binary(ObserversHtml),
    true = binary:match(ObserversBin, <<"<a href=">>) =/= nomatch,
    true = binary:match(ObserversBin, <<"<h3 class=\"content-list__title\">", CustomNotification/binary, "</h3>">>) =/= nomatch,
    {NavHtml, _} = z_render:output(z_template:render_block(content_before_body, <<"page.module.tpl">>, [{id, ParentId}], Context), Context),
    true = binary:match(iolist_to_binary(NavHtml), <<"Notifications <span>2</span>">>) =/= nomatch,
    {Updated, _} = zotonicwww2_external_import:collect_entries(
        [ModuleRow, ControllerRow, DispatchRow#{<<"rules">> => [Rule#{<<"path">> => <<"/changed/:id">>}]}], Repo, Context),
    {ok, #{updated := 1}} = zotonicwww2_doc_import:sync_external(<<"external_2147483644">>, Updated, <<"dispatch-update">>, Context),
    true = binary:match(m_rsc:p(DispatchId, body, Context), <<"/changed/:id">>) =/= nomatch,
    {Remaining, _} = zotonicwww2_external_import:collect_entries([ModuleRow#{<<"observes">> => []}, ControllerRow], Repo, Context),
    {ok, #{deprecated := 1}} = zotonicwww2_doc_import:sync_external(<<"external_2147483644">>, Remaining, <<"dispatch-removed">>, Context),
    false = m_rsc:p(DispatchId, is_published, Context),
    [] = m_edge:objects(ParentId, observes, Context),
    [] = m_rsc:p(ParentId, doc_module_observers, Context),
    ok.
