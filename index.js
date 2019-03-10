import { Elm } from './src/Main.elm';

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
