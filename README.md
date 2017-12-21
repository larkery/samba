# Samba Notes

This is a simple drum machine written to help me keep track of what
I've learned at Bristol Samba. It's done with clojurescript & reagent.

## Producing a built artefact

    $ lein cljsbuild once min

## Useful parts

If you're interested in drum synthesis with web audio in
clojurescript, look at src/samba/patch.cljs and
src/samba/instruments.cljs.

samba.patch contains wrappers around web audio for building
synthesizer graphs, and samba.instruments uses samba.patch to make
some different drum sounds. The bass drum sounds are following the
sound-on-sound bass drum FM synthesis, which you can read at
https://github.com/micjamking/synth-secrets/.

The other drums are mostly resynthesized by spectral analysis of
recordings I took.

Things I've noticed in building this are mostly:

- Web audio falls apart anywhere if you create the audio graph for
  each instrument anew when you play it, but on desktop it's OK
  if you keep a complex graph hanging about and reshape some envelopes
  when you want to play a note.
- However, web audio falls apart on mobile if your audio graph is really
  complicated and you do this.
- You can get round this by recording the sound of your synthesised drums
  with OfflineAudioContext.
- Web Audio on iOS has some stuff to prevent sound being produced
  outside of user interaction; this even seems to break things like using
  OfflineAudioContext to record sounds, so my thing doesn't work on iOS.
- Reagent/React are suprisingly quick.

The sound sequencing happens in samba.sequencer, which uses setTimeout
slightly ahead of time to schedule notes with higher precision than
setTimeout can do on its own.