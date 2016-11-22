
__Use Haskell for Distributed Systems Development__

This simple repo is designed to provide an overview of how to use haskell to do distributed systems development. The
audience is presumed to be software engineers who are unfamiliar with haskell, and indeed functional programming in
general. The goal is to show how the kinds of things software developers program can be easily, and in my view more
easily done using haskell as compared to pretty much any programming language one cares to mention.

This repo was constructed using the stack system as follows:

```
stack new use-haskell servant
```

The Servant libraries provide a very elegant method for building REST based services.

To this, we add code to demonstrate some basic functionality that developers commonly need to implement to provide a
fully functional service. Each functionality below is demonstrated in a separate REST API method:

1. The loading of environment variables.
2. The loading of command line arguments.
3. Simple file Input/Output.
4. Database writing.
5. Database search/read.
6. Call a REST service.

The code is all contained in a single file `lib.hs` in the `src` directory. The code is heavily commented from the point
of view of a non-haskell programmer, but nonetheless a reasonably experienced programmer presumed to have prior
experience with mainstream programming languages such as C++, Java or C-sharp.


