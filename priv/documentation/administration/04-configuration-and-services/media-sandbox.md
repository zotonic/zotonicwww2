---
name: "admin_media_sandbox"
title: "Configure media sandboxing and an optional media runner"
summary: "Check media-process isolation and decide whether to run conversions on a separate service."
category: "adminguide"
language: "en"
is_published: true
parent: "admin_collection_services"
order: 8
required_modules: []
source_paths: ["doc/technotes/media-sandboxing.md", "apps/zotonic_core/src/support/z_exec.erl", "apps/zotonic_core/src/media/z_media_runner.erl", "apps/zotonic_core/src/media/z_media_runner_pool.erl", "apps/zotonic_mod_base/src/controllers/controller_media_runner_callback.erl"]
zotonic_keywords: ["how_to_guide", "operator", "media_management", "security", "configuration", "api_and_integration"]
---

# Configure media sandboxing and an optional media runner

**Access needed:** Server configuration and deployment access; access to both installations when using a remote runner.

::: aside
The optional **mediarunner** site moves processing to a separate service, which is useful for keeping expensive conversions away from the web server or providing a suitable processing host.
:::

Media processing opens files supplied by users and runs tools such as ImageMagick, Ghostscript, and FFmpeg. The media sandbox limits what those commands can read, write, and access.

| Arrangement | Use it when |
| --- | --- |
| Local processing with the native sandbox | Your web host has the required tools and enough capacity. No separate runner is needed. |
| A separate mediarunner service | You want separate processing capacity, tool management, or hosts. It adds credentials, network transfers, callbacks, and another service to monitor. |

Running the runner on the same machine does not move CPU or memory pressure off that machine. Neither arrangement replaces upload permissions, backups of originals, or installation-level resource limits. Only calls using the media-profile API receive these restrictions; ordinary OS commands are not automatically sandboxed.

## 1. Verify the local sandbox

From the Zotonic checkout, connect to the running node:

```sh
bin/zotonic shell
```

At the Erlang prompt:

```erlang
z_exec:sandbox_status().
```

An `{ok, ...}` result confirms the enforcement probe succeeded on **this host**. It does not check every tool or asset path. Press Ctrl-C twice to detach the remote shell.

The `exec_sandbox` setting defaults to `required`. Missing helpers or failed sandbox setup stop processing, and a failed media command is not automatically retried without protection. However, an unsupported OS or kernel logs a notice and continues without OS isolation. Check the actual probe result and deployment logs.

Linux needs enabled Landlock ABI 3 or newer and libseccomp 2.5 or newer. Build with the required headers and development library, and deploy the native helper and its runtime dependencies. Container support depends on the host kernel and actual mounts as well as the image. macOS uses Seatbelt with different limits; Windows and BSD have no implemented native backend in this version. The [sandbox technical notes](https://github.com/zotonic/zotonic/blob/master/doc/technotes/media-sandboxing.md) describe those differences.

## 2. Check real conversions

On acceptance, upload a small image, a representative video if enabled, and a PDF if your site's policy permits PDF processing. Confirm the generated preview or conversion and inspect failures in the logs. The sandbox blocks network access for the media command; it does not change ImageMagick's own policy or enable formats that policy forbids.

If a custom font or tool is inaccessible, grant only its required paths in the trusted Zotonic application configuration. For example, an additional font directory can be granted to both image profiles:

```erlang
{exec_sandbox_profiles, #{
    imagemagick => #{read => ["/srv/media-assets/fonts"]},
    imagemagick_pdf => #{read => ["/srv/media-assets/fonts"]}
}}.
```

This is one configuration property; place it in the existing configuration structure. The directory must exist on the processing host. Profiles have separate grants; custom binaries can also need access to their libraries and configuration. Retest after applying a change. Do not disable sandboxing or grant an entire site directory merely to make a conversion succeed.

## 3. Decide whether to use mediarunner

Stay with local processing unless a separate runner solves a concrete capacity or operational problem. For a runner, plan:

1. A supported processing host with the required tools and verified sandbox enforcement.
2. An HTTPS endpoint and credentials issued by the runner, following its [installation documentation](https://github.com/zotonic/mediarunner#readme).
3. Network access from Zotonic to the runner **and back** to each client site's canonical HTTPS callback address.
4. Capacity, temporary-file storage, monitoring, and a decision about local fallback.

The runner receives media inputs and returns processing results. Treat it as a trusted service handling those files, including private uploads. Use deployment-level CPU, memory, process, and disk controls: per-command limits do not bound the aggregate load of all jobs.

## 4. Install the runner and create a consumer

On the processing host, use compatible Zotonic and mediarunner revisions. The current runner and client require protocol version 3.

1. Place the mediarunner site in `apps_user/mediarunner` in the runner's Zotonic checkout. Build the checkout with `make`. For a container deployment, follow the runner's [Docker build and deployment instructions](https://github.com/zotonic/mediarunner/blob/master/docs/docker.md), which also cover persistent volumes and the service user.
2. Install the required media tools, fonts, ImageMagick policy, and sandbox helper. Verify enforcement on this processing host.
3. Configure the hostname, database/schema, trusted HTTPS, and administrator password in private site configuration, such as `site_config.d/mediarunner/site.config` below the installation's configuration directory. The supplied site is disabled and contains no credentials. Enable it only after completing these settings, then start it through the installation's normal site-management procedure.
4. Open the runner's homepage and sign in as an administrator. Check the dashboard's sandbox status before sending files. An unsupported sandbox is shown as an alarm, not hidden behind a successful startup.
5. Select **Add website / consumer**, enter a recognizable client name, and create it.
6. Copy the generated OAuth2 key to the client's protected configuration. It is displayed only once. Use one consumer per client installation: cache access is isolated by consumer account, not by individual token.
7. Decide whether to restrict `mediarunner_callback_urls` in the runner's site configuration. The default accepts authenticated callers' valid HTTPS endpoints; an explicit list permits only those exact endpoints, and an empty list disables callbacks.

The consumer flow creates a dedicated account with permission to use mediarunner, without administration access. API credentials authorize shell commands within the selected profile, so issue them only to trusted clients. Keep the runner's configuration and data persistent across deployments.

## 5. Configure the client

For a single runner, put these settings in the **`zotonic` application section of the client’s system `zotonic.config`**, not in the client site configuration:

| Setting | Value |
| --- | --- |
| `media_runner_hostname` | The runner hostname, with a port if needed |
| `media_runner_protocol` | `https` |
| `media_runner_oauth2_key` | The token issued for this client; keep it secret |
| `media_runner_local_fallback` | `false` initially, so testing cannot silently use local processing |

For several runners, `media_runners` supplies the pool instead of the single-runner settings. Inspect an existing pool before changing the single hostname: a configured pool takes precedence, and an empty pool selects local processing.

The callback URL is generated from the client site's canonical configuration. The callback endpoint is supplied by `mod_base`; do not install the mediarunner site on every client. Check DNS, certificate trust, reverse-proxy routing, and request-size limits in both directions. For a cluster, callbacks must reach the submitting Zotonic node: pending registrations live in its memory.

Keep the `file` utility installed on the client: MIME detection still runs locally. Apply client configuration using the normal configuration/restart procedure. Drain or account for pending work before restarting the submitting node, because its pending callback registrations are lost.

## 6. Verify processing and failure behaviour

1. With local fallback off, submit a representative image conversion and a video conversion if supported.
2. Confirm that the runner accepted and processed the jobs, that callbacks arrived, and that the final preview or output is usable on the client site.
3. In acceptance, make the runner temporarily unavailable and inspect the resulting failure and recovery. Keep production jobs out of this test.
4. Enable local fallback only if the client has the required tools, capacity, and verified sandbox policy. The fallback covers runner availability failures, not every processing or configuration error.
5. Restore normal service and check one new job before declaring the change complete.

An HTTP 204 callback acknowledgement means the callback was delivered, not that the conversion succeeded. Check the job result and installed output. A successful sandbox probe on the client also says nothing about enforcement on the runner: check each processing host.

If a remote image job has no compatible runner, compare the configured ImageMagick major version and executable across the pool. For other failures, distinguish sandbox setup, tool policy, credentials, capacity, network delivery, and output validation before retrying.

## 7. Monitor capacity, cache, and credentials

Use the runner homepage for queue state, sandbox status, failures, and cache usage. Use **Consumers → Statistics** to investigate one client's workload. Heavy FFmpeg renders use a separate pool; video thumbnails and audio artwork share the general pool with image work.

Configure runner capacity in its **site configuration**. Start with the automatic general-worker capacity and the default single FFmpeg render worker, then adjust from observed CPU, memory, queue delay, and disk use. HTTP 429 can mean overload or insufficient capacity. Raising a queue limit does not add processing capacity.

Budget storage for both cached source/results and private job files. They normally live below `<data_dir>/sites/mediarunner/files/` in `mediarunner/` and `mediarunner-work/`. PostgreSQL stores persistent jobs; interrupted processing can resume after a runner restart. This does not restore pending registrations lost by a restarted client node.

After changing fonts, ImageMagick policy, or another dependency not detected automatically, increment the runner's `mediarunner_cache_version` so earlier rendered results are not reused. The processing cache is not the authoritative backup of client media originals.

For credential rotation, open **Consumers**, edit the consumer, and select **Generate a new OAuth2 key**. Saving immediately revokes its existing keys. Copy the replacement to the client and verify a new job. Renaming without selecting that option keeps the current key. Deleting a consumer revokes its keys; existing jobs and cached files follow normal retention rather than being erased immediately.

Drain outstanding jobs before upgrading Zotonic and mediarunner together. For exact settings, transfer limits, and reverse-proxy streaming requirements, use the runner's [configuration reference](https://github.com/zotonic/mediarunner/blob/master/docs/reference.md).
