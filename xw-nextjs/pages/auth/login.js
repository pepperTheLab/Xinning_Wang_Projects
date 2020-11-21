import React from 'react';
import { FormControl, TextField, Button } from '@material-ui/core';

const Login = () => (
    <FormControl>
        <TextField name={'username'} label={'Username'} type={'text'} />
        <TextField name={'password'} label={'Password'} type={'text'}/>
      <Button type='submit'>Sign in</Button>
    </FormControl>
);

export default Login;
