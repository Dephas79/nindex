%% Copyright 2024 beta
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     https://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(add_edit).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
    #template{file="./site/templates/bare.html"}.
title() -> "Add New/Edit Link".

%%*************************************************************
%% State2:
%% User enters new link info;
%% clicks Save
%% System saves new link info;
%% transitions back to initial state
%%*************************************************************
get_linkid() ->
    case wf:path_info() of 
        "new"-> new;
        ID -> wf:to_integer(ID)
    end.

body() ->
    LinkID = get_linkid(),
    Weblink = ni_links:get_link(LinkID),
    form(Weblink).

%% Entere New link form
form(new) ->
    form(new, "", "", "");

form(Weblink) ->
    ID = ni_links:id(Weblink),
    Topic = ni_links:topic(Weblink),
    Desc = ni_links:descriptor(Weblink),
    Url = ni_links:url(Weblink),
    form(ID, Topic, Desc, Url).

%% Form 4
form(ID, Topic, Desc, Url) ->
    wf:defer(save_link, topic,
    #validate{validators=[
    #is_required{text="Topic required"}]}),
    wf:defer(save_link, descriptor,
    #validate{validators=[
    #is_required{text="Descriptive text required"}]}),
    wf:defer(save_link, url,
    #validate{validators=[
    #is_required{text="URL required"}]}),

 [
    #h3 {text="Create or Edit Web Link"},
    #label{text="Topic"},
    #br{},
    #textbox{id=topic, text=Topic},
    #br{},
    #label{text="Descriptive Text"},
    #br{},
    #textbox{id=descriptor, text=Desc},
    #br{},
    #label{text="URL"},
    #br{},
    #textbox{id=url, text=Url},
    #br{},
    #button {
    id=save_link,
    text="Save",
    postback={save, ID}
    },
    #link{style="margin-left:10px", text="Cancel", url="/"}
 ].

 event({save, Linkid}) ->
    save(Linkid).

save(new) ->
    [Topic, Desc, Url] = wf:mq([topic, descriptor, url]),
    Weblink = ni_links:new(Topic, Desc, Url),
    save_and_redirect(Weblink);

save(Linkid) ->
    [Topic, Desc, Url] = wf:mq([topic, descriptor, url]),
    Weblink = ni_links:get_link(Linkid),
    Weblink2 = ni_links:topic(Weblink, Topic),
    Weblink3 = ni_links:descriptor(Weblink2, Desc),
    Weblink4 = ni_links:url(Weblink3, Url),
    save_and_redirect(Weblink4).

save_and_redirect(Weblink) ->
    ni_links:save_link(Weblink),
    wf:redirect("/").

