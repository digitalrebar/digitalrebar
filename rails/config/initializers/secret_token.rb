# Be sure to restart your server when you modify this file.

# Your secret key for verifying the integrity of signed cookies.
# If you change this key, all old signed cookies will become invalid!
# Make sure the secret is at least 30 characters and all random,
# no regular words or you'll be exposed to dictionary attacks.
Rebar::Application.config.secret_token = ENV["SECRET_TOKEN"] || 'deba4591f1a3e29dfc026c08a286ae5b357aa978b2f810709ad3afaef8cd2ad6726302a5744e349220ad438b1e7ad6233cfd40f24abfebccb1dd6befc9b9ede6'
