/*! \file
 *
 * \brief Brazilian Collect Call Detection
 *   - Detect brazilian collect call music tones 
 * 
 * \ingroup applications
 *
 * Author: Robert Oliveira, 2007
 * 
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#include "asterisk.h"

ASTERISK_FILE_VERSION(__FILE__, "$Revision: 29555 $")

#include "asterisk/file.h"
#include "asterisk/logger.h"
#include "asterisk/channel.h"
#include "asterisk/pbx.h"
#include "asterisk/dsp.h"
#include "asterisk/module.h"
#include "asterisk/options.h"

#define TONES_SIZE        10
// #define ENERGY_THRESHOLD  4.0e7
#define ENERGY_THRESHOLD  3.0e7
#define HITS_THRESHOLD    3
#define ITERATIONS        120
#define CHECKPOINT_ITERATIONS 60

static char *tdesc = "Brazilian Collect Call Detection";
static char *app = "CollectCallDetect";
static char *synopsis = "Detect brazilian collect calls by standardized music tones";
static char *descrip = 
    "  CollectCallDetect()  Detect collect calls analyzing audio tones \n"
    "specified by Norma Telebras 201-200-702 and reafirmed by Consulta \n"
    "Publica Anatel 109. \n"
    "  Jump to \"collect\" extension on positive detection\n";

STANDARD_LOCAL_USER;

LOCAL_USER_DECL;

/* Some routines by Steven Underwood as published under the zapata library */
/*
  tone_detect.c - General telephony tone detection, and specific
  detection of DTMF.

  Copyright (C) 2001  Steve Underwood <steveu@coppice.org>

  Despite my general liking of the GPL, I place this code in the
  public domain for the benefit of all mankind - even the slimy
  ones who might try to proprietize my work and use it to my
  detriment.
*/

typedef struct {
    float v2;
    float v3;
    float fac;
    int hits;
} goertzel_state_t;

static inline void goertzel_sample(goertzel_state_t *s, short sample)
{
    float v1;
    float fsamp  = sample;

    v1 = s->v2;
    s->v2 = s->v3;
    s->v3 = s->fac * s->v2 - v1 + fsamp;
}

static inline void goertzel_update(goertzel_state_t *s, short *samps, int count)
{
    while (count--) {
        goertzel_sample(s,*(samps++));
    }
}

static inline float goertzel_result(goertzel_state_t *s)
{
    return s->v3 * s->v3 + s->v2 * s->v2 - s->v2 * s->v3 * s->fac;
}

static inline void goertzel_init(goertzel_state_t *s, float freq, int samples)
{
    s->v2 = s->v3 = 0.0;
    s->fac = 2.0 * cos(2.0 * M_PI * (freq / 8000.0));
    s->hits = 0;
}

static inline void goertzel_reset(goertzel_state_t *s)
{
    s->v2 = s->v3 = 0.0;
}


static const float tones[TONES_SIZE] =  {
    523.248f, 1046.496f, 587.328f, 1174.656f, 659.248f, 
    1318.496f, 698.464f, 1396.928f, 783.984f, 1567.968f };

static int collectcalldetect(struct ast_channel *chan) 
{
    struct ast_frame *f;
    int old_rfmt = 0;
    int res = 0;
    int iterations = ITERATIONS;
    int distinct_hits = 0;
    int detected=0;
    struct ast_dsp *sildet=NULL;        /* silence detector dsp */
    goertzel_state_t state[TONES_SIZE];
    goertzel_state_t *st;
    float energy;
    int i;

    /* initialize goertzel structures */
    for ( i=0; i<TONES_SIZE; i++ ) {
        goertzel_init(&(state[i]), tones[i], 102);
    }

    /* set to linear mode and create the silence detector */
    old_rfmt = chan->readformat; 
    res = ast_set_read_format(chan, AST_FORMAT_SLINEAR);
    if (!res) {
	sildet = ast_dsp_new(); 
	if (sildet!=NULL) {
	    ast_dsp_set_threshold(sildet, 256);
	} else {
	    ast_log(LOG_WARNING, "Unable to create silence detector :(\n");
	    res=-1;
	}
    } else {
	ast_log(LOG_WARNING, "Unable to set to linear mode, giving up\n");
    } 
    

    /* wating tones... */
    if ( !res ) {
	while ( iterations-- && !detected ) {
              
	    /* wait for audio */
	    res = ast_waitfor(chan, 2000);
	    if (res<0) {
		res = ast_waitfor(chan, 2000);
		if (res<0) {
		    ast_log(LOG_WARNING, "No audio available on %s\n", chan->name);
		    res=-1;
		    break;
		}
	    }

	    f = ast_read(chan);
	    if ( f!=NULL && (f->frametype == AST_FRAME_VOICE) ) {
            
		/* process samples */
		for ( i=0; i<TONES_SIZE; i++ ) {
		    goertzel_update(&(state[i]), f->data, f->datalen/2);
		}
            
		/* check energy rised by every tone */
		i=TONES_SIZE;
		st=state;
		while (i--) {
		    energy = goertzel_result(st);
		    if ( energy > ENERGY_THRESHOLD ) {
			if ( st->hits==0 ) {
			    distinct_hits++;
			    detected = (distinct_hits > HITS_THRESHOLD);
			    iterations = CHECKPOINT_ITERATIONS;
			} 
			st->hits++;
#if EXTRA_DEBUG
			ast_log(LOG_WARNING, "Tone detected with energy %f in channel '%s' \n", energy, chan->name);
			ast_log(LOG_WARNING, "Distinct hits detected %i in channel '%s' \n", distinct_hits, chan->name);
#endif
		    }
		    goertzel_reset(st); /* prepare for next frame */
		    st++;
		}
	    }
	    if ( f!=NULL ) ast_frfree(f);
	}
    }

    if ( sildet!=NULL ) ast_dsp_free(sildet);

    if ( old_rfmt ) {
        if ( ast_set_read_format(chan, old_rfmt) ) {
            ast_log(LOG_WARNING, "Unable to restore format %s to channel '%s'\n", 
                    ast_getformatname(old_rfmt), chan->name);
        }
    }

    if ( detected ) {
        ast_log(LOG_WARNING, "Brazilian collect call detected\n");
        ast_goto_if_exists(chan, chan->context, "collect", 1);
    }

    return detected;
}

static int collectcalldetect_exec(struct ast_channel *chan, void *data)
{
    int res = 1;
    struct localuser *u;

    LOCAL_USER_ADD(u);
    res = collectcalldetect(chan);
    LOCAL_USER_REMOVE(u);
    if (res > 0) {
        res = 0;
    }

    return res;
}

int unload_module(void)
{
    int res;

    res = ast_unregister_application(app);
    STANDARD_HANGUP_LOCALUSERS;
    return res;
}

int load_module(void)
{
    return ast_register_application(app, collectcalldetect_exec, synopsis, descrip);
}

char *description(void)
{
    return tdesc;
}

int usecount(void)
{
    int res;
    STANDARD_USECOUNT(res);
    return res;
}

char *key()
{
    return ASTERISK_GPL_KEY;
}

