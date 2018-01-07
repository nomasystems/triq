##==============================================================================
## Copyright 2010 Trifork A/S
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
## http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##==============================================================================

.PHONY: clean edoc test dialyze check eunit ci ci-dialyze pages

REBAR=`sh -c "PATH='$(PATH)':util which rebar3||util/getrebar||echo false"`

all:
	@$(REBAR) compile

test: eunit

ci: clean test

ci-dialyze: clean dialyze

pages: edoc
	@test -d pages/out/edoc && rm pages/out/edoc/* || mkdir -p pages/out/edoc
	@cp doc/*.html doc/*.css doc/*.png pages/out/edoc
	@(cd pages && ./export)
	@rm -f pages/out/*.html~

edoc:
	@$(REBAR) edoc

clean:
	@$(REBAR) clean

eunit:
	@$(REBAR) eunit

dialyze:
	@$(REBAR) dialyzer
