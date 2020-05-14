
.PHONY: site bs

all: site bs3

site:
	sass -C --sourcemap=none --update site.scss:../../lib/css/site.css

bs3:
	sass -C --sourcemap=none --update bs3/assets/stylesheets/_bootstrap.scss:../../lib/css/bs3.css
