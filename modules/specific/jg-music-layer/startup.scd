SuperDirt.start;
MIDIClient.init;
~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
~midiOut.latency = 0.45;
~dirt.soundLibrary.addMIDI(\midi, ~midiOut);
