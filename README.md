# .emacs.d

This is my personal emacs configuration.

Everything here is borrowed or stolen. Help yourself.

Goals:

- Deterministic
    - Git subtrees for all dependencies
    - no network access required
- Start as fast as possible
    - `use-package` defers as much as possible

## Usage

```
git clone https://github.com/thumphries/.emacs.d
ln -s "$(pwd)/.emacs.d" "${HOME}/.emacs.d"
./compile
```

## Subtrees

Adding new subtrees:

```sh
# ./subtree-add <remote> [<ref>]
./subtree-add https://github.com/abo-abo/hydra master
```

Updating subtrees:

```sh
# ./subtree-pull <remote> [<ref>]
./subtree-pull https://github.com/abo-abo/hydra master
```

## Profiling

```sh
./profile
```

## Recommended

A few things that have worked really well:

### Subtrees

Everything is checked into [site-lisp](./site-lisp) using git
subtrees.

I haven't had to think about emacs packages in anger since I started
doing this.

Sometimes a package will break when I upgrade emacs itself, almost
always resolved with a subtree pull or a new dependency.

### Lazy loading

See [use-package](https://github.com/jwiegley/use-package).

With byte compilation and occasional startup profiling everything
starts up in the order of 200ms.

This is not a common thing to need. I use emacs completely wrong -
kinda like it's vi with lots of sessions and without the GUI. Fast
startup is pretty useful for this workflow.

### Garbage collector thresholds

The default emacs garbage collection threshold is/was rather
low. Doesn't matter much for text editing, but at session init time
we're interpreting webapp quantities of random bytecode.

Bumping the collection threshold during init and dropping it afterward
led to a substantial reduction in init time. The actual numbers are
not magic, I probably pasted them from somewhere. Their magnitude is
what matters.

```elisp
;; init.el prologue
(setq gc-cons-threshold (* 50 1000 1000))

;; init.el
; ...

;; init.el epilogue
(setq gc-cons-threshold (* 2 1000 1000))
```
