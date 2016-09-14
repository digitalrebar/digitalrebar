class stdlib {
  exec { "stdlib-stub":
    command => "/bin/echo 'stdlib here!'",
    logoutput => true
  }
}
