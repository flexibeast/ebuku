# ebuku - Interface to the buku Web bookmark manager

*Author:* Alexis <flexibeast@gmail.com>, Erik Sjöstrand <sjostrand.erik@gmail.com><br>
*Version:* 0<br>

Ebuku provides a basic interface to the
[buku](https://github.com/jarun/buku) Web bookmark manager.

![Image of the EBuku UI](ebuku.png)<br>

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Customisation](#customisation)
- [TODO](#todo)
- [Issues](#issues)
- [License](#license)

## Installation

Install [Ebuku from MELPA](https://melpa.org/#/ebuku), or put the
`ebuku` folder in your load-path and do a `(load "ebuku")`.

## Usage

Create an Ebuku buffer with <kbd>M-x ebuku</kbd>.

In the `*Ebuku*` buffer, the following bindings are available:

* <kbd>s</kbd> - Search for a bookmark (`ebuku-search`).

* <kbd>r</kbd> - Show recently-added bookmarks (`ebuku-search-on-recent`).

* <kbd>*</kbd> - Show all bookmarks (`ebuku-show-all`).

* <kbd>-</kbd> - Toggle results limit (`ebuku-toggle-results-limit`).

* <kbd>g</kbd> - Refresh the search results, based on last search (`ebuku-refresh`).

* <kbd>RET</kbd> - Open the bookmark at point in a browser (`ebuku-open-url`).

* <kbd>n</kbd> - Move point to the next bookmark URL (`ebuku-next-bookmark`).

* <kbd>p</kbd> - Move point to the previous bookmark URL (`ebuku-previous-bookmark`).

* <kbd>a</kbd> - Add a new bookmark (`ebuku-add-bookmark`).

* <kbd>d</kbd> - Delete a bookmark (`ebuku-delete-bookmark`).  If point is on
  a bookmark, offer to delete that bookmark; otherwise, ask for the
  index of the bookmark to delete.

* <kbd>e</kbd> - Edit a bookmark (`ebuku-edit-bookmark`).  If point is on a
  bookmark, edit that bookmark; otherwise, ask for the index of the
  bookmark to edit.

* <kbd>q</kbd> - Quit Ebuku.

Bindings for Evil are available via the
[evil-collection](https://github.com/emacs-evil/evil-collection)
package, in `evil-collection-ebuku.el`.

### Completion

Ebuku provides two cache variables for use by completion frameworks
(e.g. Ivy or Helm): `ebuku-bookmarks` and `ebuku-tags`, which can
be populated via the `ebuku-update-bookmarks-cache` and
`ebuku-update-tags-cache` functions, respectively.

## Customisation

The `ebuku` customize-group can be used to customise:

* the path to the `buku` executable;

* the path to your `bookmarks.db` file;

* the number of recently-added bookmarks to show;

* which bookmarks to show on startup;

* the maximum number of bookmarks to show;

* whether to automatically retrieve URL metadata when adding a
  bookmark; and

* the faces used by Ebuku.

## TODO

* One should be able to edit bookmarks directly in the `*Ebuku*`
  buffer, à la `wdired`.  Much of the infrastructure to support this
  is already in place, but there are still important details yet to
  be implemented.

<a name="issues"></a>

## Issues / bugs

If you discover an issue or bug in Ebuku not already
noted:

* as a TODO item, or

* in [the project's "Issues" section on
  GitHub](https://github.com/flexibeast/ebuku/issues),

please create a new issue with as much detail as possible,
including:

* which version of Emacs you're running on which operating system,
  and

* how you installed Ebuku.

## License

[GNU General Public License version
3](https://www.gnu.org/licenses/gpl.html), or (at your option) any
later version.


---
Converted from `ebuku.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
