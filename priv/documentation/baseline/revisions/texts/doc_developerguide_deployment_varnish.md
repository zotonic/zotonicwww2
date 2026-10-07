# Cache selected public assets with Varnish

Use a supported Varnish installation and its matching VCL reference. Terminate HTTPS in the public proxy; Varnish's ordinary HTTP backend configuration does not itself provide that TLS endpoint. Keep the backend and management interface private.

Start with an explicit public asset policy. This VCL 4.1 example passes everything except anonymous requests under `/lib/`:

```vcl
vcl 4.1;

backend default {
    .host = "127.0.0.1";
    .port = "8000";
}

sub vcl_recv {
    if (req.method != "GET" && req.method != "HEAD") {
        return (pass);
    }
    if (req.http.Authorization || req.http.Cookie || req.http.Upgrade) {
        return (pass);
    }
    if (req.url !~ "^/lib/") {
        return (pass);
    }
}
```

Let the built-in VCL process the remaining request and backend response. Do not strip `Set-Cookie`, override private/no-store responses, or force a positive TTL onto them. Keep media, API, admin, login and MQTT/WebSocket traffic out of this initial cache policy; the outer proxy should route WebSocket upgrades directly to Zotonic.

Validate the configuration with the installed Varnish version before loading it. Test an anonymous asset twice, then authenticated pages and a response with `Cache-Control: private` or `no-store`. Confirm only intended public content becomes a cache hit. Add page or media caching only after reviewing Zotonic's visibility, cache headers and invalidation requirements.

The old VCL 3 callbacks (`vcl_fetch`, `vcl_error`) and `req.request` field are obsolete. Use the [current Varnish documentation](https://varnish-cache.org/docs/) for the version you deploy.
