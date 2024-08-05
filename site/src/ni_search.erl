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

-module(ni_search).
-export([search/1]). 

search(SearchString) ->
    Weblinks = ni_links:get_all(),
    [Link || Link <- Weblinks, filter(SearchString, Link)].

%% A filter function
filter(SearchString, Weblink) ->
    Topic = ni_links:topic(Weblink),
    Descriptor = ni_links:descriptor(Weblink),
    SeachWords = unique_words(SearchString),
    WeblinkWords = unique_words(Topic ++ " " ++ Descriptor),
    SharedWords = shared(SeachWords, WeblinkWords),
    sets:size(SharedWords) > 0.

%% Delete duplicate words
unique_words(String) ->
    Tokens = string:lexemes(String, " "),
    sets:from_list(Tokens).

%% Shared helper function
shared(S1, S2) ->
    sets:intersection(S1, S2).

