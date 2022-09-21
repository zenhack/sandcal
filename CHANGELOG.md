# 0.9.1

* Introduce a small margin around the week view, to avoid text being cut
  off.

# 0.9

* Misc. style changes to make parts of the UI less heavy.

# 0.8

* Increase font size on inputs to avoid zoom on iOS
* Improvements to the week view
  * Add a heading with the date for the week being shown
  * Make events take up space proportional to their length
  * Always expand the grid to fill horizontal space; this avoids
    changing size from week to week.

# 0.7

* Add an extremely rudimentary week view.

# 0.6.1

* Events with the empty string (`""`) as a summary are displayed as
  "Untitled event" -- just like events with *no* summary.

# 0.6

* Event end times are now displayed on the event page and on the
  upcoming page.

# 0.5

* Slightly better app icon.
* Fix a minor display bug with all-day events
  (<https://github.com/zenhack/sandcal/issues/26>)

# 0.4

* When displaying events, mark up email addresses and URLs as actual
  links.

# 0.3

* Allow specifying intervals for repeat rules, e.g. "every 2 weeks"
  instead of just "every week." We could already display these correctly
  if they were imported via an .ics, but there was no UI for adding
  them.
* Allow adding multiple repeat rules for an event.
* Fixed a spelling error in some of the UI text.

# 0.2

* The new event form is now responsive for mobile devices.
* Fixed a bug where the upcoming events page would sometimes display
  the wrong date for events (<https://github.com/zenhack/sandcal/issues/19>)

# 0.1

* It is now possible to share a calendar in read-only mode.
* You can now export to an .ics file
* The user's time zone is now auto-detected; the corresponding settings
  dialog has been removed.
* The upcoming events page now adds (today) or (tomorrow) annotations
  where relevant.

# alpha-1

First release.
