Forked from (https://github.com/seagle0128/.emacs.d/releases/latest)
Add my own features:
1. CMake Projects
   - Add .dir-locals.el and .clang-format to your PROJECT_DIR,set *((nil . ((cmake-ide-build-dir . "PROJECT_DIR/build"))))*
   - cd build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
   - Then,you can M-x cmake-ide-compile
2. Pyim
## FAQ

1. Why is the modline messy?

    Powerline fonts or all-the-icons are missing on your system. Please install
    [powerline-fonts](https://github.com/powerline/fonts) for `telephone-line` or
    run `M-x all-the-icons-install-fonts` for `doom-modeline`.

1. How to search Chinese via pinyin?

    In Emacs, `C-s :`. If you just want to search `:`, use `C-s \:`.

1. How to use the Centaur Dashboard?

    Set `(setq centaur-dashboard t)` in `~/.emacs.d/custom.el`. Dashboard will
    be opened at startup. After startup, you could use `F2` to reopen it anytime.
    In the dashboard, you could easily jump to Homepage(`H`), Restore
    Session(`R`), Edit Config (`E`), Update(`U`), Recent Files (`r`),
    Bookmarks(`m`) and Projects(`p`).

1. Does Centaur Emacs support Language Server Protocol (LSP)?

    LSP is supported and enabled by default in Centuar Emacs now. `eglot` is the
    default client, and `lsp-mode` is another choice. Before use it you should
    install language servers as below. Use `(setq centaur-lsp nil)` to disable
    `LSP` if you don't like it.
    - Golang: `go get -u github.com/sourcegraph/go-langserver`
    - Python: `pip install python-language-server`
    - Ruby:  `gem install solargraph`
    - Javascript/Typescript: `npm i -g javascript-typescript-langserver`
    - CSS: `npm i -g vscode-css-languageserver-bin`
    - HTML: `npm i -g vscode-html-languageserver-bin`
    - Bash/Shell: `npm i -g bash-language-server`. Require Python2.5+, use
      `--python` to specify.
    - C/C++/Objective-C : `brew install cquery` or dwonload binary from
      [here](https://github.com/cquery-project/cquery/releases).
    - Rust: `rustup component add rls-preview rust-analysis rust-src`
    - Java:
      ``` shell
      wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
      tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
      ```
    - PHP: refer to the [installation
      guide](https://github.com/felixfbecker/php-language-server#installation).
      ``` shell
      composer require felixfbecker/language-server
      composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
      ```

1. How to enable `plantuml` in `org-mode`?

    Put `(setq org-plantuml-jar-path "<path of plantumx.x.x.jar>")` in `custom.el`.

1. Why the Emacs environment variables and `exec-path` are different between GUI
   and terminal?

    Please refer to #33. You should instead set environment variables in startup
    files like .profile, .bash_profile or .zshenv, then `Centaur Emacs` is able
    to recoginze and import the environment variables.
