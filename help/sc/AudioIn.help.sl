# AudioIn --  read audio input from analogue-to-digital converter

_AudioIn(channel=1)_

Reads audio from the sound input hardware.

- channel: input channel number to read. Channel numbers begin at 1.

Stereo through patching from input to output:

		AudioIn([1, 2])

