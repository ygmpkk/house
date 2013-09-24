/* mpn_divrem_1(quot_ptr, qsize, dividend_ptr, dividend_size, divisor_limb) --
   Divide (DIVIDEND_PTR,,DIVIDEND_SIZE) by DIVISOR_LIMB.
   Write DIVIDEND_SIZE limbs of quotient at QUOT_PTR.
   Return the single-limb remainder.
   There are no constraints on the value of the divisor.

   QUOT_PTR and DIVIDEND_PTR might point to the same limb.

Copyright (C) 1991, 1993, 1994, 1996, 1998, 1999, 2000 Free Software
Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"



/* __gmpn_divmod_1_internal(quot_ptr,dividend_ptr,dividend_size,divisor_limb)
   Divide (DIVIDEND_PTR,,DIVIDEND_SIZE) by DIVISOR_LIMB.
   Write DIVIDEND_SIZE limbs of quotient at QUOT_PTR.
   Return the single-limb remainder.
   There are no constraints on the value of the divisor.

   QUOT_PTR and DIVIDEND_PTR might point to the same limb. */

#ifndef UMUL_TIME
#define UMUL_TIME 1
#endif

#ifndef UDIV_TIME
#define UDIV_TIME UMUL_TIME
#endif

static mp_limb_t
#if __STDC__
__gmpn_divmod_1_internal (mp_ptr quot_ptr,
	      mp_srcptr dividend_ptr, mp_size_t dividend_size,
	      mp_limb_t divisor_limb)
#else
__gmpn_divmod_1_internal (quot_ptr, dividend_ptr, dividend_size, divisor_limb)
     mp_ptr quot_ptr;
     mp_srcptr dividend_ptr;
     mp_size_t dividend_size;
     mp_limb_t divisor_limb;
#endif
{
  mp_size_t i;
  mp_limb_t n1, n0, r;
  int dummy;

  /* ??? Should this be handled at all?  Rely on callers?  */
  if (dividend_size == 0)
    return 0;

  /* If multiplication is much faster than division, and the
     dividend is large, pre-invert the divisor, and use
     only multiplications in the inner loop.  */

  /* This test should be read:
       Does it ever help to use udiv_qrnnd_preinv?
	 && Does what we save compensate for the inversion overhead?  */
  if (UDIV_TIME > (2 * UMUL_TIME + 6)
      && (UDIV_TIME - (2 * UMUL_TIME + 6)) * dividend_size > UDIV_TIME)
    {
      int normalization_steps;

      count_leading_zeros (normalization_steps, divisor_limb);
      if (normalization_steps != 0)
	{
	  mp_limb_t divisor_limb_inverted;

	  divisor_limb <<= normalization_steps;
	  invert_limb (divisor_limb_inverted, divisor_limb);

	  n1 = dividend_ptr[dividend_size - 1];
	  r = n1 >> (BITS_PER_MP_LIMB - normalization_steps);

	  /* Possible optimization:
	     if (r == 0
	     && divisor_limb > ((n1 << normalization_steps)
			     | (dividend_ptr[dividend_size - 2] >> ...)))
	     ...one division less... */

	  for (i = dividend_size - 2; i >= 0; i--)
	    {
	      n0 = dividend_ptr[i];
	      udiv_qrnnd_preinv (quot_ptr[i + 1], r, r,
				 ((n1 << normalization_steps)
				  | (n0 >> (BITS_PER_MP_LIMB - normalization_steps))),
				 divisor_limb, divisor_limb_inverted);
	      n1 = n0;
	    }
	  udiv_qrnnd_preinv (quot_ptr[0], r, r,
			     n1 << normalization_steps,
			     divisor_limb, divisor_limb_inverted);
	  return r >> normalization_steps;
	}
      else
	{
	  mp_limb_t divisor_limb_inverted;

	  invert_limb (divisor_limb_inverted, divisor_limb);

	  i = dividend_size - 1;
	  r = dividend_ptr[i];

	  if (r >= divisor_limb)
	    r = 0;
	  else
	    {
	      quot_ptr[i] = 0;
	      i--;
	    }

	  for (; i >= 0; i--)
	    {
	      n0 = dividend_ptr[i];
	      udiv_qrnnd_preinv (quot_ptr[i], r, r,
				 n0, divisor_limb, divisor_limb_inverted);
	    }
	  return r;
	}
    }
  else
    {
      if (UDIV_NEEDS_NORMALIZATION)
	{
	  int normalization_steps;

	  count_leading_zeros (normalization_steps, divisor_limb);
	  if (normalization_steps != 0)
	    {
	      divisor_limb <<= normalization_steps;

	      n1 = dividend_ptr[dividend_size - 1];
	      r = n1 >> (BITS_PER_MP_LIMB - normalization_steps);

	      /* Possible optimization:
		 if (r == 0
		 && divisor_limb > ((n1 << normalization_steps)
				 | (dividend_ptr[dividend_size - 2] >> ...)))
		 ...one division less... */

	      for (i = dividend_size - 2; i >= 0; i--)
		{
		  n0 = dividend_ptr[i];
		  udiv_qrnnd (quot_ptr[i + 1], r, r,
			      ((n1 << normalization_steps)
			       | (n0 >> (BITS_PER_MP_LIMB - normalization_steps))),
			      divisor_limb);
		  n1 = n0;
		}
	      udiv_qrnnd (quot_ptr[0], r, r,
			  n1 << normalization_steps,
			  divisor_limb);
	      return r >> normalization_steps;
	    }
	}
      /* No normalization needed, either because udiv_qrnnd doesn't require
	 it, or because DIVISOR_LIMB is already normalized.  */

      i = dividend_size - 1;
      r = dividend_ptr[i];

      if (r >= divisor_limb)
	r = 0;
      else
	{
	  quot_ptr[i] = 0;
	  i--;
	}

      for (; i >= 0; i--)
	{
	  n0 = dividend_ptr[i];
	  udiv_qrnnd (quot_ptr[i], r, r, n0, divisor_limb);
	}
      return r;
    }
}



mp_limb_t
#if __STDC__
mpn_divrem_1 (mp_ptr qp, mp_size_t qxn,
	      mp_srcptr np, mp_size_t nn,
	      mp_limb_t d)
#else
mpn_divrem_1 (qp, qxn, np, nn, d)
     mp_ptr qp;
     mp_size_t qxn;
     mp_srcptr np;
     mp_size_t nn;
     mp_limb_t d;
#endif
{
  mp_limb_t rlimb;
  mp_size_t i;

  /* Develop integer part of quotient.  */
  rlimb = __gmpn_divmod_1_internal (qp + qxn, np, nn, d);

  /* Develop fraction part of quotient.  This is not as fast as it should;
     the preinvert stuff from __gmpn_divmod_1_internal ought to be used here
     too.  */
  if (UDIV_NEEDS_NORMALIZATION)
    {
      int normalization_steps;

      count_leading_zeros (normalization_steps, d);
      if (normalization_steps != 0)
	{
	  d <<= normalization_steps;
	  rlimb <<= normalization_steps;

	  for (i = qxn - 1; i >= 0; i--)
	    udiv_qrnnd (qp[i], rlimb, rlimb, 0, d);

	  return rlimb >> normalization_steps;
	}
      else
	/* fall out */
	;
    }

  for (i = qxn - 1; i >= 0; i--)
    udiv_qrnnd (qp[i], rlimb, rlimb, 0, d);

  return rlimb;
}
