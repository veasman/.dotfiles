# TODO


# general

build out rofi replacement tool
build out screenshotting tool using rofi

# vwm

* config options not working:
workspace_count, default_layout

they are valid config options but they dont do anything in the code

i took out padding_y because it was useless, but we do need some way of adding some vertical space between the bar background and the module pills when bar is in float mode but transparent_background is false
bar modules get some margin_x by default, i dont want this, if my modules margin_x is 10 and my gap is 10 then my bar modules and windows should line up on screen if that makes sense
think on bar module coloring, user confiured? preset? etc etc
consider custom bar modules
truncate something when too much in bar
allowing keybinds like XF86AudioRaiseVolume so i can setup default media keys in the config
bar modules for brightness and media?

* config options i do not even want:
default_layout

i only like the current layout, i dont want to provide other layouts

## kitty

create ability to yank text to clipboard without using mouse?

## loom
* loom conf to support incoming vwm bar
* what changes need to be made in loom to work with vwm's new config system and vwmbar?
* expand loom config for picom or any other tools that need more granularity
* why does firefox/floorp break when applying a theme from loom

## nvim

* why does nvim handle enter and backspace as double presses when not inside tmux
* e.g. when i press backspace it registers the keydown and key up both as individual key presses or something like that
* can i make tmux scrollable?

## vwm wayland
things to add when migration to wayland:

* compositor/render pipeline
* xdg-shell window management
* output management
* seat/input handling
* app-id based rules
* layer-shell support
* XWayland support
* IPC/control socket
* bar state export model
* popup/utility window rules
* config reload path
* theme/state integration points for Loom

and potentially:

* bar/client separation plan
* utility window behavior
* decorations strategy
* damage/frame handling that feels smooth
* enough protocol coverage that core desktop tools work


## Gardening

### Last Spring Frost

* Leawood, KS: Apr 15
* Centralia, IL: Apr 15

### Plants

* honeydew
    * When: 1-2 weeks after last frost
    * Water: keep soil moist while seeds germinate
    * Soil: fertilized, well-draining with lots of organic matter (compost, aged manure, etc)
    * Plot: raised bed, 4-6' apart
    * Sunlight: as much as possible
* cucumber
    * When: 1-2 weeks after last frost
    * Water: keep soil moist, water more once plant starts to flower
    * Soil: well-draining with organic matter (compost, nutrient meals, etc)
    * Plot: raised bed, 3" apart, 6-9" apart if using a trellis
    * Sunlight: as much as possible
* tomatoes
    * When: 1-2 weeks after last frost
    * Water: keep soil moist
    * Soil: rich, well-draining with lots of organic matter (compost, aged manure, etc)
    * Plot: raised bed, 18-36" apart; stage or cage
    * Sunlight: as much as possible
* peppers
    * When: not until overnight temps are above 55 consistently
* garlic
    * When: usually in fall, can plant in early spring
    * Soil: cut off flower shoots that grow in spring, can inhibit growth
* potatoes
    * beginning of spring
    * Soil: loose, slightly acidic
* onions
    * When: early spring
    * Watering: roughly 1-2" of rain a week worth of water
* raspberries
    * When: early spring
    * Water: keep soil moist but not soggy
    * Soil: rich, well-draining soil with compost
    * Plot: 2-3' apart, use trellis
    * Sunlight: full sun

