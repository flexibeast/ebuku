*Author:* Alexis <flexibeast@gmail.com><br>
*Version:* 0<br>

EBuku provides a basic interface to the
[Buku](https://github.com/jarun/Buku) Web bookmark manager.

![Image of the EBuku UI](ebuku.png)<br>

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Customisation](#customisation)
- [TODO](#todo)
- [Issues](#issues)
- [License](#license)

## Installation

Put the `ebuku` folder in your load-path and do a `(load "ebuku")`.

## Usage

Create an EBuku buffer with <kbd>M-x ebuku</kbd>.

In the `*EBuku*` buffer, the following bindings are available:

* <kbd>a</kbd> - Add a new bookmark (`ebuku-add-bookmark`).

* <kbd>d</kbd> - Delete a bookmark (`ebuku-delete-bookmark`).  If point is on
  a bookmark, offer to delete that bookmark; otherwise, ask for the
  index of the bookmark to delete.

* <kbd>e</kbd> - Edit a bookmark (`ebuku-edit-bookmark`).  If point is on a
  bookmark, edit that bookmark; otherwise, ask for the index of the
  bookmark to edit.

* <kbd>s</kbd> - Search for a bookmark (`ebuku-search`).

* <kbd>r</kbd> - Show recently-added bookmarks (`ebuku-search-on-recent`).

* <kbd>g</kbd> - Refresh the search results, based on last search (`ebuku-refresh`).

* <kbd>q</kbd> - Quit EBuku.

## Customisation

The path to the `buku` executable, the number of bookmarks shown by
`ebuku-search-on-recent`, and the faces used by EBuku, can be
customised via the `ebuku` customize-group.

## TODO

* One should be able to edit bookmarks directly in the `*EBuku*`
  buffer, à la `wdired`.  Much of the infrastructure to support this
  is already in place, but there are still important details yet to
  be implemented.

<a name="issues"></a>

## Issues / bugs

If you discover an issue or bug in EBuku not already
noted:

* as a TODO item, or

* in [the project's "Issues" section on
  GitHub](https://github.com/flexibeast/ebuku/issues),

please create a new issue with as much detail as possible,
including:

* which version of Emacs you're running on which operating system,
  and

* how you installed EBuku.

## License

[GNU General Public License version
3](http://www.gnu.org/licenses/gpl.html), or (at your option) any
later version.


---
Converted from `ebuku.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
