#!/bin/sh
cd `dirname $0`
./rebar compile
exec erl -pa $PWD/apps/*/ebin -boot start_sasl -config files/app.config -s roller -tempile root '"files/site/templates/"' -roller document_root '"files/site/www/"'