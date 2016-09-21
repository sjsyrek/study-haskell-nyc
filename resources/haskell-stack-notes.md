# Haskell Stack Notes

## Installation

Visit the [Stack website](https://docs.haskellstack.org/en/stable/README/) for the most up-to-date instructions for installing Haskell using the Stack platform. Avoid using other installation solutions, including any variant of the Haskell platform.

For Mac OS X users who have [Homebrew](http://brew.sh/):

- `brew install haskell-stack`

On other Unix and Unix-like systems, you can generally install directly by using one of the following commands:

- `curl -sSL https://get.haskellstack.org/ | sh`
- `wget -qO- https://get.haskellstack.org/ | sh`

Windows users should consult the [documentation](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).

If you accidentally installed the Haskell Platform or have an old installation haunting your machine, you can try following [these instructions](https://mail.haskell.org/pipermail/haskell-cafe/2011-March/090170.html) or using [this code](https://gist.github.com/steakknife/3775443).

## Setting up a project

For basic experimentation and doing exercises, it is recommended that you just create a directory for your code files and use the global Stack installation, which will run automatically when you type `stack ghc` for the compiler or `stack ghci` for the REPL.

To set up a full project, enter the following commands:

1. `stack new project-name` where `project-name` is whatever you want to call your project
2. `cd project-name` to switch to the new directory that Stack will have created for your project
3. `stack setup` to create a basic project framework
4. `stack build` to compile the project
5. `stack exec project-name-exe` to run the compiled output

## Upgrading Stack or GHC

- `stack update` will update your packages, but you don't generally need to do this manually.
- `stack upgrade` will reinstall Stack from source, including GHC.
- `stack exec -- ghc --version` will tell you what version of GHC you are using.
- `stack --resolver ghc-8.01 setup --reinstall` will upgrade your installation of GHC without rebuilding Stack itself (or install whichever version you prefer).

If you want to use a specific version of GHC for your global Stack installation, edit your `~/.stack/global-project/stack.yaml` file, and set the resolver to whichever version prefer, for example:

- `resolver: lts-6.11` for GHC 7.10.3
- `resolver: lts-7.0` for GHC 8.0.1

For project-specific installations of Stack, edit your project's local `stack.yaml` file instead.

## Shell auto-completions

Add the following line(s) to your shell's configuration dotfile (e.g. `.bash_profile` or `zshrc`) to enable support for shell tab completion for standard Stack arguments.

### bash

`eval "$(stack --bash-completion-script stack)"`

### zsh

```sh
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"
```

## Shell shortcuts

If typing `stack ghc` or `stack ghci` is annoying you, add the following aliases to your shell dotfile:

```sh
alias ghci="stack exec -- ghci"
alias ghc="stack exec -- ghc"
```
