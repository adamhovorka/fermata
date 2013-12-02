Fermata
=======

Fermata is a software defined synthesizer.


Usage
-----

- `make` - That's basically it. It "compiles" the song into a WAV file and then plays it.

### Other Make Targets

- `make clean` - Removes all of the "compiled" files
- `make wav` - "Compiles" the WAV file
- `make play` - Just plays the WAV file
- `make mp3` - Encodes the WAV file as MP3
- `make ogg` - Encodes the WAV file as OGG


Dependencies
------------

- `make`
- `perl`
- `sox`
- Chicken Scheme

### Optional Dependencies

- `lame` - MP3 encoding
- `oggenc` - OGG encoding
