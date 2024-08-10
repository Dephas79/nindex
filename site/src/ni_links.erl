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

-module(ni_links).
-export([
            init_db/0,
            get_all/0,
            get_link/1,
            save_link/1,
            update_link/2,
            delete_link/1,
            new/3,
            id/1,id/2,
            topic/1,topic/2,
            descriptor/1,descriptor/2,
            url/1,url/2
]). 

%% THE FACADE
%% Make a macro to be our alias for the backend
%%-define(DB, ni_joedb).
-define(DB, ni_dets).

init_db() ->
    ?DB:init_db().

%% wrap up the facade
get_all() ->
    ?DB:get_all().
get_link(ID) ->
    ?DB:get_link(ID).
save_link(Weblink) ->
    ?DB:save_link(Weblink).
update_link(NewWeblink, Data) ->
    ?DB:update_link(NewWeblink, Data).
delete_link(ID) ->
    ?DB:delete_link(ID).

%% the getters
new(Topic, Descriptor,Url) ->
    ?DB:new(Topic, Descriptor, Url).
id(Weblink) ->
    ?DB:id(Weblink).
topic(Weblink) ->
    ?DB:topic(Weblink).
descriptor(Weblink) ->
    ?DB:descriptor(Weblink).
url(Weblink) ->
    ?DB:url(Weblink).

%% the setters
id(Weblink, ID) ->
    ?DB:id(Weblink, ID).
topic(Weblink, Topic) ->
    ?DB:topic(Weblink, Topic).
descriptor(Weblink, Desc) ->
    ?DB:descriptor(Weblink, Desc).
url(Weblink, Url) ->
    ?DB:url(Weblink, Url).

