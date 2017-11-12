# tagist
Made for bloging, but meant to be flexible beyond this

# Usage
We use the fragment identifier (including the leading hash) to decide which part of all the gists to show.
Forward slash (/) act as an "and" operator.
Every condition between forward slashes will be present in the results.
Conditions may be preceeded by either a comma (,) or an octothorp (#).
Octothorps filter by users, and a string of characters not preceded by an octothorp is a "tag".
The condition matches either the string of characters in the description, or the user.
Commas provied an "or" mechanism.
The result will contain at least one of the elements separated by a comma.

# Results
Results display the title as a link to the gist on github, and user(s) with a link to their profile on github.
Files are listed alphabetically, but markdown files are first.
File names are links to the relevant github url.
Filenames that this application understands are preceeded by an arrow pointing right.
Clicking the arrow points it downwards and displays the contents of the file inline.

Currently the only file types supported in this manner are markdown files.

# License
Copyright © 2017 Jon Allen <ylixir@gmail.com>
This work is free. You can redistribute it and/or modify it under the
terms of the Do What The Fuck You Want To Public License, Version 2,
as published by Sam Hocevar. See the COPYING file for more details.