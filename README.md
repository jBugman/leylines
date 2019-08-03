## Dev build

    yarn global add elm elm-format elm-analyse elm-live

    elm-live src/Main.elm --start-page=static/index.html --port=3000 -- --output=static/main.js

## Production build

    yarn global add uglify-js

    uglifyjs static/main.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=static/main.js

    git subtree push --prefix dist origin gh-pages
