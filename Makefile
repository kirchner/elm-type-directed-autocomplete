build:
	elm make src/Main.elm --output=elm.js

link:
	npm run cli && npm link
