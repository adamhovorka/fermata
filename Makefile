CC=csi
CFLAGS=-q
FILE=song.scm

PLAY=play -q
SAMPLERATE=48000

all: wav play

wav: raw
	sox -r $(SAMPLERATE) -e unsigned -b 8 -c 1 $(FILE:.scm=.raw) $(FILE:.scm=.wav)

raw:
	$(CC) $(CFLAGS) $(FILE) | perl -pe 's/([0-9]+)/sprintf "%02x", $$1/ge' | perl -ne 's/([0-9a-f]{2})/print chr hex /gie' > $(FILE:.scm=.raw)

play:
	$(PLAY) $(FILE:.scm=.wav)

mp3: wav
	lame -V 5 $(FILE:.scm=.wav) $(FILE:.scm=.mp3)

ogg: wav
	oggenc -q 3 -o $(FILE:.scm=.ogg) $(FILE:.scm=.wav)

clean:
	rm -f *.raw *.wav *.mp3 *.ogg
