Scheme library implementing [FastCGI](https://en.wikipedia.org/wiki/FastCGI)

## Simple example
### Scheme Server
    (import (scheme base)
            (scheme write)
            (retropikzel scgi))

    (handle-request
      '((port . "3001"))
      (lambda (request)
        (display "Content-type: text/html")
        (display "\r\n")
        (display "\r\n")
        (display "Hello world")))

### HTTP Server

Using lighttpd might be the simplest to get started, install it and then
put this into file called lighttpd.conf in your project folder.

    server.document-root = "/your-project-path"
    server.errorlog = "/tmp/scgi-error.log"
    server.modules = ("mod_scgi")

    server.port = 3000
    scgi.debug = 1
    scgi.server = ("/" =>
                    (( "host" => "127.0.0.1",
                       "port" => 3001,
                       "check-local" => "disable")))

    mimetype.assign = (
                    ".html" => "text/html",
                    ".txt" => "text/plain",
                    ".jpg" => "image/jpeg",
                    ".png" => "image/png")

Run ligghtpd:

    lighttpd -D -f lighttpd.conf

Then run your FCGI porgram and open your browser to http://127.0.0.1:3000/
