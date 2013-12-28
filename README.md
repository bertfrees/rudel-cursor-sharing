[rudel-cursor-sharing][]
========================
Extension of the Obby protocol in [Rudel][] for sharing cursor positions.

Installation
------------
### Manual installation
Clone rudel-cursor-sharing and add the root directory to your load-path: 
```lisp
(add-to-list 'load-path "/path/to/rudel-cursor-sharing")
```

### el-get
el-get users can install rudel-cursor-sharing with the following recipe:
```lisp
(:name rudel-cursor-sharing
       :type git
       :url "http://github.com/bertfrees/rudel-cursor-sharing.git"
       :checkout "alpha"
       :depends (rudel))
```

### package.el
rudel-cursor-sharing is also available as an ELPA package. First you
need to add [bertfrees.github.com/elpa](http://bertfrees.github.com/elpa)
to the list of repositories:
```lisp
(add-to-list 'package-archives '("bertfrees" . "http://bertfrees.github.com/elpa/packages/"))
(package-install 'rudel-cursor-sharing)
```

Usage
-----
```lisp
(eval-after-load 'rudel
  '(load "rudel-cursor-sharing.el"))
```

License
-------
Copyright 2013 [Bert Frees][bert]

This program is free software: you can redistribute it and/or modify
it under the terms of the [GNU General Public License][gpl]
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.


[rudel-cursor-sharing]: http://github.com/bertfrees/rudel-cursor-sharing
[rudel]: https://github.com/scymtym/rudel
[bert]: http://github.com/bertfrees
[gpl]: http://www.gnu.org/licenses/gpl.html
