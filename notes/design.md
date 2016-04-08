# Funweb

Funweb is a web application system. It's inspired by code I wrote for
wigflip.com. Wigflip consisted of several different mini-applications
that had different interfaces and produced different kinds of images
or other amusements.

Wigflip had some organizational problems. Each mini-application,
developed months and even years apart, tackled similar problems in
different ways.

* Configuration - often done with hardcoded special variables
* Template management
* Output management - each mini-app produced images that were
  presented to the user
* Parameter and request handling

Funweb aims to make it easy to produce that kind of site again,
without reinventing the same tools for each application all over
again.

But it's not meant to be limited to that model of site - the goal is
to make it easy to spin up a quick web service without thinking to
hard about it.

## Object interaction

Funweb is organized as a single server object that has one or more
apps. Each app has one or more handlers.

The server is responsible for accepting requests from clients.

For each request, the server iterates over each app and calls the
app's dispatch-fun with the request as the argument. If the
dispatch-fun returns a handler, the handler is called with the request
and the return value is used by the server to create a response.

The handler's return value can be a simple string or a structured
plist list. In the case of a string, the server constructs a text/html
response and returns it. In the case of a list, the first element, a
keyword, indicates what kind of response it is.

Supported handler response types:

* :error - the value is the error message, with optional :backtrace and
:code arguments
* :file - the value is a pathname, which is used by the server to
  create the body of the response. Optional :content-type argument is
  used by the server to set the content-type
* :redirect - the value is the new location, optional :permanently
  indicates whether it's a permanent redirect (301 status code) or
  temporary (302 status code)

These structured lists aren't created directly, but by functions such
as MAKE-FILE-RESPONSE, MAKE-REDIRECTION-RESPONSE,
MAKE-NOT-FOUND-RESPONSE, etc. That way handler functions don't have to
care about the structure of the lists.

This also makes it possible to call handler functions interactively
and see reasonable results. For example, a handler that needs to
redirect does not call Hunchentoot redirection functions
directly. Instead, it calls MAKE-REDIRECTION-RESPONSE, which returns a
human-readable list that indicates to the server how to produce the
real response by calling the appropriate Hunchentoot functions.

In theory, this should make it easier to switch backends.

The default app structure is meant for handling static URLs with CL
functions. DEFINE-HANDLER takes a request method and path and has code
that is called when the app matches the request with its
dispatch-fun.

For example:

    (define-app my-app ())
    
    (define-handler my-app (:get "hello") () "Hello, world")

The () in the above example is an opportunity to bind passed
parameters to local variables.

For example:

	(define-handler my-app (:get "greeting") (name)
	  (format nil "Hello, ~A!" (html-escape name))

DEFINE-HANDLER could be extended to take more options to change how
the dispatch-fun is informed of the handler.

    (define-handler my-app :regexp (:get "t:(.*)") (tag)
      ...code to handle the request, with TAG bound to the register match...

This isn't implemented, yet.

## Configuration

The goal with configuration is to be able to relocate an app, both on
disk and in an URL space, without changing any code. Only the config
file changes.

For example, in a test environment, the Lisp web server may be
responsible for serving all content, including static resources like
scripts and images, but in the production environment, static
resources are likely handled by another server like Apache or nginx.

Handler code, therefore, is concerned about *partial*, *relative*
paths that are made complete when a configuration is applied. The
configuration supplies information about the hostname, port, and path
prefix, if any.

If a handler's response refers to other URLs in the application, it
has to use relative paths, or query the system for complete
information.


### Configuration mechanism

The server and its apps must be configured before the server can be
started and accept requests. Attempting to use unconfigured apps or
servers will result in an `object-not-configured` error.

An object's configuration may be applied directly with plists by
calling `(configure object plist)`.

There's also `configure-from-file`, which will load the plist as the
first form of a file and use it to configure the object. When a server
object is configured from a file, it configures all its apps by
loading config files in `apps/<app-name>`, merging with the original
server config file to get the absolute pathname and the file type.


