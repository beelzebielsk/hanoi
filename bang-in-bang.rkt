; I've got a couple options for this.
; My original way:
; ================
;
; Make individual things that would work with big bang. Compose a
; series of pieces together. When one piece closes, the next piece
; starts up with the final state (or some subset of it) of the
; previous piece. Composition requires each component being designed
; to feed into the next component (or each component being designed to
; take from the previous, whatever your point of view).
; 
; I might be able to make this work by creating a new big-bang
; function which takes in a series of big-bang functions. This
; big-bang function would hold an additional piece of state which
; records which bang-thing is currently in use. As one bang-thing
; ends, this overlord bang-thing would:
; - hold the final state of the current bang-thing in it's own state.
; - Alter it's own state to show that the next bang-thing is now
;   rendering.
; - Give this bang-thing the final state of the previous bang-thing.

; NOTE: consider setting up component state to have "control state",
; which really belongs only to the component, and is invisible to the
; rest of the world, and "component state" which is what the next
; component will get. This way a component can use it's state to
; manage itself freely without having to worry about the information
; that the next component would see.
;
; Something more like React:
; ==========================
; Make individual things whose behaviors could be controlled through
; passing in some parameters.
;
; Make bang-things composable in the sense that one bang-thing could
; render other bang-things. So, instead of having each component
; knowing about each other, you'd offload the "know about each other"
; burden to one "parent component".
;
; The children components could alter the Parent's state so that the
; parent can make new decisions based on the actions of the children.
;
; However, there's a slight kink in this idea: when react expands JSX,
; it (ought to) expand it to HTML that would go in a page (or alter
; DOM in equivalent way). The way to expand/render something is
; obvious. However, for this, the way to render something is not
; obvious. I could...
; - Make it so that everything has some default behavior, like append
;   horizontally, or vertically, or overlay on top of one another. In
;   a scheme like this, the order in which a bang-thing is specified
;   is important.
; - Give the parent the ability to control what happens. It could be
;   that, much like any normal bang-thing, the parent is a bang-thing,
;   and thus has a to-draw (render) function, and the to-draw function
;   must produce an image, created from the images of each underlying
;   function.
;
; NOTE: The React thing is sort-of a general version of my 1st idea.
; And after thinking about the react way... it could handle some of
; the scenarios that I'm most concerned about. The 1st idea is about a
; series of almost-independent pieces whose publically exposed
; "interface" is the set of acceptable initial states. Each component
; is responsible for sending the next component to be rendered, and
; respecting the "interface" of the next component. But, how would a
; quick menu screen work? I'm in a game, I press Start, a menu comes
; up that's not the full size of the screen. I do something there,
; then I finish. How do I return back to my game? The menu has to
; conjure up the game? How would that work, would I have to shove the
; game into initial state of the menu? This is a case I'd like to
; handle, and it looks ugly in this idea. It's going to something, and
; then back to a previous thing that's looking pretty bad.
;
; Q: Going back to this menu idea, how would I pause a game? An
; "active" game requires responding to ticks... I suppose that I could
; stop reporting ticks to a "non-active" component? Lemme not think
; about that right now.
