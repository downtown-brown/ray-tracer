# ray-tracer

A ray tracer written in Clojure following [Ray Tracing in One Weekend](https://raytracing.github.io/books/RayTracingInOneWeekend.html#positionablecamera)

## Usage

This project uses Leiningen, run it with a simple

    $ lein run

## Closing thoughts

I think it was pretty clear from the beginning that Clojure wasn't the right choice for this project. At least in its basic form, it just doesn't have the performance required. I'm sure there are ways to get the performance closer to the reference C++ program, but at that point why not just write Java? This was my first project with Clojure, and I found it interesting, but I don't think the programs I typically write are well suited for it. Who knows, maybe I'll end up using it more in the future.