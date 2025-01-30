# tiles

## Different users

In the following, we will divide people in 3 categories : 
1. Artists, who want to write pieces of art with UMedia
2. Programmers, who want to create their own media language with UMedia
3. Contributors, who are interested in building UMedia itself

For artists, no Haskell programmation skills should be required. Programmers and Contributors need to know Haskell.

There should be : 
1. A generic tutorial on the syntax for artists
2. One tutorial for each Langugage for artists, written by the creator of that Language
3. A tutorial on how to make your own language for programmers
4. A full documentation for both contributors and programmers

## How to use

Testing 

`` stack test ``

Generating a Sierpinski fractal in SVG

`` stack build --exec sierpinski-exe ``

## Architecture

UMedia uses a 4 layers architecture : 
1. Pattern, which handles the data
1. Syntax/Tiles, which defines the syntax of UMedia
1. Language/Primitives, which defines the words of UMedia
1. Renderer, which specifies how to render a UMedia program

Let's see how people should interact with UMedia :
1. Artists ony care about Syntax and Languages. They need a good and intuitive documentation for both of them.
2. Programmers are the one who write Languages and Renderers. They should understand the syntax, but they shouldn't care about the details in Pattern.
3. Contributors are mostly interested in Pattern and Syntax, and understand this whole architecture

Here is a description of the interface of each layer
1. Pattern is an implementation of hierarchical patterns. This is a container, very close to a Rose tree. It should provide :
    * a way to navigate threw a pattern
    * a way to render a pattern
1. Syntax/Tiles allows to program with Patterns. It relies on Tiles.  It should offer a syntax to create a pattern from scratch, especially it should provide
    * a way to combine Patterns via Tiles
    * a way to explicit the hierarchy
1. Language/Primitives define which Atoms, Transforms and Space can be used in a Language. For now there are 3 Languages : Vector2D, Music and String. A language should provide
    * UMedia words to create atoms
    * UMedia words to transform Media
1. Renderer implements a render function of a Media in some specific Language. For instance, one could implement an SVG or a PostScript renderer for the Language Vector2D. It should provide
    * a render function specific to some Language





