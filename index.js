import { Elm } from './src/Main.elm';

//  Copyright 2019 Fabian Kirchner
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.


const cacheKey = "elm-type-directed-autocomplete"

var app = Elm.Main.init({
  flags: {
    localStorage: localStorage.getItem(cacheKey) || ""
  },
  node: document.getElementById("elm-container")
});

app.ports.cache.subscribe(function(data) {
  localStorage.setItem(
    cacheKey,
    JSON.stringify(data)
  );
});
