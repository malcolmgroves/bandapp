BandApp
=======
Malcolm G: beta 13 – working


The idea behind BandApp was to provide a sample band app that let's people browse
News, Videos, Upcoming Shows and details about a band, in this case, Five Eyed Hand,
Mikey Hendersen's band.

Further, the idea was to make it easy for someone to adapt this to a different band. As
such, BandApp's content is mostly driven by the config.ini file deployed in the bundle.

Overview
========
The main UI for BandApp follows the Facebook-style UI, with the main screen sliding to
the right when the "hamburger" button in the top left is pressed, revealing a drawer of
content.

In this case, the drawer contains a menu that allows you select different pages to be
displayed on the main screen. This menu is driven by the [Pages] section of config.ini,
with each entry matching to a later section in config.ini that provides more detail. The
menu is actually loaded into a TObjectList<TPageMenuItem> and then LiveBindings is used to
bind this to a Listbox with a custom styled List Item.

The main content area is simply a layout (lytPageHost) that has different pages (TFrames
actually) parented onto it depending on which menu has been selected.

I have created a few pages:

-fRSSFeedPage   : takes it's content from an RSS Feed defined in config.ini and displays it
                  in a listbox. For each item you can share it to twitter, etc using the
                  ShareSheet functionality, and also open the original item in a browser
                  control. This page is used for the News and Upcoming shows menus.

-fVimeoFeedPage : takes it's content from an RSS Feed of  Vimeo album defined in config.ini
                  and displays the title and image in a listbox. For each item you can share
                  it to twitter, etc using the ShareSheet functionality, and also play the
                  video by opening the vimeo page in a browser control and pressing play.
                  This page is used for the Videos menu.

NB: both of these store their content in a collection and bind it using LiveBindings.

-fAboutPage     : takes it's content from the config.ini file. The Tabs entry int he [About]
                  sections is a comma-seperated list of tabs, and each one has a correspodning
                  entry for the tab text and tab image. The image files are deployed in the
                  bundle with the app. You can swipe up and down on the text to scroll it, and
                  swiping left/right on the image moves you between the tabs. This page, not
                  surprisingly, is used for the About 5EH menu.

-fDebugPage     : This is just a test page I was using to make sure the whole page/params/methods
                  thing was working. Uncomment the Debug entry in the [Pages] section and it'll
                  show up in the menu.

There are lots of things I learnt during the process of building this app, so i'll probably blog
about various bits of it in the coming weeks.
