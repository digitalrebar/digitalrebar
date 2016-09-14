include stdlib

exec { "test-hello-world":
  command => "/bin/echo 'hello world!'",
  logoutput => true
}