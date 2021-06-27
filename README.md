# Running PhotoGroove

`npx elm-live src/PhotoGroove.elm  --start-page=serve.html -- --output app.js`

# Running PhotoFolders

`npx elm-live src/PhotoFolders.elm  --start-page=photo-folders.html -- --output app.js`

# Running Main
`npx elm-live src/Main.elm  --start-page=index.html -- --output app.js`


# http-server-spa

This [application](https://www.npmjs.com/package/http-server-spa) is
used to serve up static assets when we use multiple routes.

`npx http-server-spa .`




# Development setup

## Emacs LSP integration

There is something wrong in the newest version of the elm language
server 2.1.0 that causes syntax errors. I found [this
thread](https://github.com/elm-tooling/elm-language-server/issues/598)
which describes how to fix the problem temporarily.

1. Remove global installation of elm-language server `npm uninstall @elm-tooling/elm-language-server`
1. Check out elm-language-server e3610b1
1. Install the checked out version globally `npm install ~/workspace/external/elm-language-server/`
