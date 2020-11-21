import React from 'react'
import { signIn, signOut, useSession } from 'next-auth/client'
import { Button } from '@material-ui/core';
import Link from 'next/link'

const Home = () => {
  const [ session, loading ] = useSession();

  return (
      <>
        {!session && <>
          Not signed in <br/>
          <Link href={'/auth/login'}>
            <Button>Link</Button>
          </Link>
        </>}
        {session && <>
          Signed in as {session.user.email} <br/>
          <Button onClick={signOut}>Sign out</Button>
        </>}
      </>
  )
};

export default Home;
