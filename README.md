Frankie is a simple Web server and routing framework. The server lets your run
your app in your custom monad, not just IO. The framework is
configuration-driven and ensure that controllers are well-typed (vs. frameworks
like Sinatra where you'd have to parse URL query parameters).

Below is a simple illustrative example (at least until this README is actually written).

There are two (very simple) example apps, which can be built & run with

```console
$ stack build --flag frankie:build-examples
$ stack exec example-hello-world     # Located at examples/HelloWorld.hs
$ stack exec example-hello-frankie   # Located examples/HelloFrankie.hs
```
