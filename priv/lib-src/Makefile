
# all: ../lib/css/bootstrap.css ../lib/css/bootstrap-print.css ../lib/css/tinymce-zotonic.css
all: ../lib/css/bootstrap.css ../lib/css/tinymce-zotonic.css

../lib/css/bootstrap.css: bootstrap-sass-3.4.1/assets/stylesheets/* \
	bootstrap-sass-3.4.1/assets/stylesheets/bootstrap/* \
	*.scss \
	site/* \
	tufte/*
	sass -q bootstrap.scss ../lib/css/bootstrap.css

../lib/css/bootstrap-print.css: bootstrap-sass-3.4.1/assets/stylesheets/* \
	bootstrap-sass-3.4.1/assets/stylesheets/bootstrap/* \
	*.scss \
	site/* \
	tufte/*
	sass -q bootstrap-print.scss ../lib/css/bootstrap-print.css

../lib/css/tinymce-zotonic.css: tinymce-zotonic.scss
	sass -q tinymce-zotonic.scss ../lib/css/tinymce-zotonic.css
