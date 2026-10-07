# Put Zotonic behind nginx

Configure Zotonic's backend listener on a private interface, for example loopback port 8000. Set the site's public hostname and HTTPS port to the values visitors use, and set `proxy_allowlist` to the actual proxy address(es). Consult `zotonic.config.in` for listener versus advertised-port settings.

In nginx's `http` context, define:

```nginx
map $http_upgrade $connection_upgrade {
    default upgrade;
    '' close;
}
```

Inside the site's existing TLS `server` block, with its real certificate and key:

```nginx
location / {
    proxy_pass http://127.0.0.1:8000;
    proxy_http_version 1.1;
    proxy_set_header Host $host;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header X-Forwarded-For $remote_addr;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection $connection_upgrade;
    proxy_read_timeout 3600s;
    client_max_body_size 100m;
}
```

This example assumes nginx is the single public proxy; it replaces an untrusted inbound forwarded-address header. A CDN or extra proxy hop needs an explicit trusted-chain configuration. Set the upload limit to your site's policy, and use the current nginx TLS defaults and your deployment's certificate policy instead of an old copied cipher list.

Run `nginx -t` before reloading. Test public pages, redirects, login, uploads and a WebSocket connection lasting beyond the usual HTTP idle timeout. Check the client address and scheme seen by Zotonic. [nginx's WebSocket documentation](https://nginx.org/en/docs/http/websocket.html) explains the upgrade headers and idle timeout.
