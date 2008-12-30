/* play-sample.c -- code to play sound samples, borrowed from GNOME libs
   $Id$

   Maybe I should have just linked against libgnome, but I don't want
   to depend on any solely GNOME libraries. ESD and libaudiofile are
   widely available (and they're not _required_ anyway)

   XXX need to make sure WORDS_BIGENDIAN is defined on big-endian systems */

#include "sawfish.h"

#ifndef GLIB_MAJOR_VERSION		/* Have we got <glib.h> ? */
# define g_malloc malloc
# define g_free free
# define g_new0(a,b) calloc(b, sizeof (a))
# define g_warning printf
#endif /* GLIB_MAJOR_VERSION */

int gnome_sound_sample_load(const char *sample_name, const char *filename);
void gnome_sound_play (const char * filename);
void gnome_sound_init(const char *hostname);
void gnome_sound_shutdown(void);

/* from gnome-libs/libgnome/gnome-sound.c HEAD as of March 12, 2000 */

/*
 * Copyright (C) 1997-1998 Stuart Parmenter and Elliot Lee
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
 */

#include "config.h"

#if 0
#include "libgnome.h"
#include "gnome-sound.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#ifdef HAVE_ESD
#include <esd.h>
#endif

int gnome_sound_connection = -1;

typedef struct _sample
  {
    int rate, format, samples, id, size;
    short *data;
  }
GnomeSoundSample;

#ifndef HAVE_LIBAUDIOFILE
typedef struct _WAVFormatChunk
  {
    char chunkID[4];
    int chunkSize;

    unsigned int dwSamplesPerSec;
    unsigned int dwAvgBytesPerSec;
    short wFormatTag;
    unsigned short wChannels;
    unsigned short wBlockAlign;
    unsigned short wBitsPerSample;
  }
WAVFormatChunk;

#ifdef WORDS_BIGENDIAN
#define SWAP_SHORT( x ) x = ( ( x & 0x00ff ) << 8 ) | ( ( x >> 8 ) & 0x00ff )
#define SWAP_LONG( x ) x = ( ( ( x & 0x000000ff ) << 24 ) |\
( ( x & 0x0000ff00 ) << 8 ) |\
( ( x & 0x00ff0000 ) >> 8 ) |\
( ( x & 0xff000000 ) >> 24 ) )
#endif

/**
 * gnome_sound_sample_load_wav:
 * @file: filename to try loading a WAV file from.
 * 
 * Used to load a .wav file into esound.
 *
 * Returns a GnomeSoundSample or NULL if file did not exist.
 *
 */
static GnomeSoundSample *
gnome_sound_sample_load_wav(const char *file)
{
#ifdef HAVE_ESD
  FILE *f;
  GnomeSoundSample *s;
  char buf[4];
  WAVFormatChunk fmt;
  int skipl = 0;
  int skipr = 0;
  char bytes = 0;
  char stereo = 0;
  int len;

  /* int                 count; */

  f = fopen (file, "r");
  if (!f)
    return NULL;
  s = g_malloc (sizeof (GnomeSoundSample));
  if (!s)
    {
      fclose (f);
      return NULL;
    }
  s->rate = 44100;
  s->format = ESD_STREAM | ESD_PLAY;
  s->samples = 0;
  s->data = NULL;
  s->id = 0;
  fread (buf, 1, 4, f);
  if ((buf[0] != 'R') ||
      (buf[1] != 'I') ||
      (buf[2] != 'F') ||
      (buf[3] != 'F'))
    {
      /* not a RIFF WAV file */
      fclose (f);
      g_free (s);
      return NULL;
    }
  fread (buf, 1, 4, f);
  fread (buf, 1, 4, f);
  fread (fmt.chunkID, 1, 4, f);
  fread (&(fmt.chunkSize), 1, 4, f);

#ifdef WORDS_BIGENDIAN
  SWAP_LONG (fmt.chunkSize);
#endif

  if ((fmt.chunkID[0] == 'f') &&
      (fmt.chunkID[1] == 'm') &&
      (fmt.chunkID[2] == 't') &&
      (fmt.chunkID[3] == ' ') &&
      16 == fmt.chunkSize)
    /* fmt chunk */
    {
      fread (&(fmt.wFormatTag), 1, 2, f);
      fread (&(fmt.wChannels), 1, 2, f);
      fread (&(fmt.dwSamplesPerSec), 1, 4, f);
      fread (&(fmt.dwAvgBytesPerSec), 1, 4, f);
      fread (&(fmt.wBlockAlign), 1, 2, f);
      fread (&(fmt.wBitsPerSample), 1, 2, f);
#ifdef WORDS_BIGENDIAN
      SWAP_SHORT (fmt.wFormatTag);
      SWAP_SHORT (fmt.wChannels);
      SWAP_LONG (fmt.dwSamplesPerSec);
      SWAP_LONG (fmt.dwAvgBytesPerSec);
      SWAP_SHORT (fmt.wBlockAlign);
      SWAP_SHORT (fmt.wBitsPerSample);
#endif

      if (fmt.wFormatTag != 1)
	{
	  /* unknown WAV encoding format - exit */
	  fclose (f);
	  g_free (s);
	  return NULL;
	}
      skipl = 0;
      skipr = 0;
      bytes = 0;
      stereo = 0;
      if (fmt.wChannels == 1)
	s->format |= ESD_MONO;
      else if (fmt.wChannels == 2)
	{
	  stereo = 1;
	  s->format |= ESD_STEREO;
	}
      else
	{
	  stereo = 1;
	  s->format |= ESD_STEREO;
	  if (fmt.wChannels == 3)
	    {
	      skipl = 0;
	      skipr = 1;
	    }
	  else if (fmt.wChannels == 4)
	    {
	      skipl = 0;
	      skipr = 2;
	    }
	  else if (fmt.wChannels == 4)
	    {
	      skipl = 0;
	      skipr = 2;
	    }
	  else if (fmt.wChannels == 6)
	    {
	      skipl = 3;
	      skipr = 1;
	    }
	  else
	    {
	      /* unknown channel encoding */
	      fclose (f);
	      g_free (s);
	      return NULL;
	    }
	}
      s->rate = fmt.dwSamplesPerSec;
      if (fmt.wBitsPerSample <= 8)
	{
	  bytes = 1;
	  s->format |= ESD_BITS8;
	}
      else if (fmt.wBitsPerSample <= 16)
	s->format |= ESD_BITS16;
      else
	{
	  /* unknown bits encoding encoding */
	  fclose (f);
	  g_free (s);
	  return NULL;
	}
    }
  for (;;)
    {
      if (fread (buf, 1, 4, f) &&
	  fread (&len, 4, 1, f))
	{
#ifdef WORDS_BIGENDIAN
	  SWAP_LONG (len);
#endif

	  if ((buf[0] != 'd') ||
	      (buf[1] != 'a') ||
	      (buf[2] != 't') ||
	      (buf[3] != 'a'))
	    fseek (f, len, SEEK_CUR);
	  else
	    {
	      s->data = g_malloc (len);
	      if (!s->data)
		{
		  fclose (f);
		  g_free (s);
		  return NULL;
		}
	      if ((skipl == 0) && (skipr == 0))
		{
		  fread (s->data, len, 1, f);
#ifdef WORDS_BIGENDIAN
		  if (fmt.wBitsPerSample > 8 && fmt.wBitsPerSample <= 16)
		    {
		      char *tmp;
		      char tmpval;
		      int i;

		      tmp = (char *) (s->data);

		      for (i = 0; i < len; i++)
			{
			  tmpval = tmp[i];
			  tmp[i] = tmp[i + 1];
			  tmp[i + 1] = tmpval;
			}
		    }
#endif
		}
	      else
		{
		}
	      s->samples = len;
	      if (stereo)
		s->samples /= 2;
	      if (!bytes)
		s->samples /= 2;
	      fclose (f);
	      return s;
	    }
	}
      else
	{
	  fclose (f);
	  return NULL;
	}
    }
  fclose (f);
  g_free (s);
  if (s->data)
    g_free (s->data);
#endif

  return NULL;
}
#endif

#if defined(HAVE_LIBAUDIOFILE) && defined(HAVE_ESD)
#include <audiofile.h>

static GnomeSoundSample *
gnome_sound_sample_load_audiofile(const char *file)
{
  AFfilehandle in_file;
  GnomeSoundSample *s;
  int in_format, in_width, in_channels;
  double in_rate;
  int bytes_per_frame;
  AFframecount frame_count, frames_read;

  int out_bits, out_channels, out_rate;
  int out_mode = ESD_STREAM, out_func = ESD_PLAY;
  esd_format_t out_format;

  in_file = afOpenFile(file, "r", NULL);
  if(!in_file)
    return NULL;

  frame_count = afGetFrameCount(in_file, AF_DEFAULT_TRACK);
  in_channels = afGetChannels(in_file, AF_DEFAULT_TRACK);
  in_rate = afGetRate (in_file, AF_DEFAULT_TRACK);
  afGetSampleFormat (in_file, AF_DEFAULT_TRACK, &in_format, &in_width);
  if (in_width == 8)
    out_bits = ESD_BITS8;
  else if (in_width == 16)
    out_bits = ESD_BITS16;
  else {
      g_warning ("only sample widths of 8 and 16 supported");
      return NULL;
  }

  bytes_per_frame = in_width / 8;

  if (in_channels == 1)
    out_channels = ESD_MONO;
  else if (in_channels == 2)
    out_channels = ESD_STEREO;
  else {
      g_warning ("only 1 or 2 channel samples supported");
      return NULL;
  }

  out_format = out_bits | out_channels | out_mode | out_func;

  out_rate = (int) in_rate;

  s = g_new0 (GnomeSoundSample, 1);

  s->rate = out_rate;
  s->format = out_format;
  s->samples = frame_count;
  s->data = g_malloc(frame_count * in_channels * bytes_per_frame);
  s->id = 0;

  frames_read = afReadFrames(in_file, AF_DEFAULT_TRACK, s->data,
			     frame_count * in_channels);

  afCloseFile(in_file);

  return s;
}
#endif


/**
 * gnome_sound_sample_load:
 * @sample_name: the name of the sample
 * @filename: the filename where the audio is stored
 *
 * Loads the audio on @filename and XXXX
 *
 * Returns: a sample_id, or a negative number otherwise.
 */
int
gnome_sound_sample_load(const char *sample_name, const char *filename)
{
#ifdef HAVE_ESD
  GnomeSoundSample *s = NULL;
  int sample_id;
  int size;
  int confirm = 0;

  if(gnome_sound_connection < 0)
    return -2;

  if(!filename || !*filename)
    return -2;

#ifdef HAVE_LIBAUDIOFILE
  s = gnome_sound_sample_load_audiofile(filename);
#else
  s = gnome_sound_sample_load_wav(filename);
#endif
  if(s)
    goto playsamp;

  /* XXX: Add handlers for more formats here */

 playsamp:
  if (!s)
    return -1;

  size = s->samples;
  if (s->format & ESD_STEREO)
    size *= 2;
  if (s->format & ESD_BITS16)
    size *= 2;

  if (gnome_sound_connection >= 0)
    {
      if (s->data)
	{
	  /* "name" of all samples is currently "E", should be name of sound 
	   * file, or event type, for later identification */
	  s->id = esd_sample_cache (gnome_sound_connection, s->format, s->rate,
				    size, (char *)sample_name);
	  write (gnome_sound_connection, s->data, size);
	  confirm = esd_confirm_sample_cache (gnome_sound_connection);
	  if (s->id <= 0 || confirm != s->id)
	    {
	      g_warning ("error caching sample <%d>!\n", s->id);
	      s->id = 0;
	    }
	  g_free (s->data);
	  s->data = NULL;
	}
    }

  sample_id = s->id;

  g_free(s->data); g_free(s);

  return sample_id;
#else
  return -1;
#endif
}

/**
 * gnome_sound_play:
 * @filename: file containing the sound sample
 *
 * Plays the audio stored in @filename
 */
void 
gnome_sound_play (const char * filename)
{
#ifdef HAVE_ESD
  char buf[23];
  int sample;

  if(gnome_sound_connection < 0) return;

  srand(time(NULL));
#ifdef HAVE_SNPRINTF
  snprintf(buf, sizeof(buf), "%d-%d", getpid(), rand());
#else
  sprintf(buf, "%d-%d", getpid(), rand());
#endif
  sample = gnome_sound_sample_load (buf, filename);

  if (sample >= 0)
  {
      esd_sample_play(gnome_sound_connection, sample);
      fsync (gnome_sound_connection);
      esd_sample_free(gnome_sound_connection, sample);
  }
  else
      g_warning ("can't load sample: %s\n", filename);
#endif
}

/**
 * gnome_sound_init:
 * @hostname: hostname where esd daemon resides.
 *
 * Initialize esd connection
 */
void
gnome_sound_init(const char *hostname)
{
#ifdef HAVE_ESD
  if(gnome_sound_connection < 0)
    gnome_sound_connection = esd_open_sound((char *)hostname);
#endif
}

/** 
 * gnome_sound_shutdown:
 *
 * shuts down the gnome sound support
 */
void
gnome_sound_shutdown(void)
{
#ifdef HAVE_ESD
	if(gnome_sound_connection >= 0){
		esd_close(gnome_sound_connection);
		gnome_sound_connection = -1;
	}
#endif
}

/* sawfish code */

DEFUN("primitive-play-sample", Fprimitive_play_sample,
      Sprimitive_play_sample, (repv filename), rep_Subr1)
{
    rep_DECLARE1 (filename, rep_STRINGP);
    gnome_sound_play (rep_STR (filename));
    return Qt;
}

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("sawfish.wm.util.play-sample");
    if (!batch_mode_p ())
	gnome_sound_init ("");
    rep_ADD_SUBR (Sprimitive_play_sample);
    return rep_pop_structure (tem);
}

void
rep_dl_kill (void)
{
    if (!batch_mode_p ())
	gnome_sound_shutdown ();
}
