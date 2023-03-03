# ChatGPT Mode

A package for integrating chatgpt within Emacs

# Instructions

Call `chatgpt-input-auth-token` and input the authorisation token supplied when you call `https://chat.openai.com/api/auth/session` endpoint when you login into chatGPT.

Call `chatgpt-run` and input your response and the output will be in an org mode buffer with executable org-babel code blocks

# Update
As of 11/12/2022, OpenAI has added Cloudflare protections that make it more difficult to access the unofficial API.

Therefore this wrapper is broken until I can find a way around that.

# TODO
- [ ] Add Contributing guidelines
- [ ] [Update the package to match the new api](https://github.com/hhamud/ChatGPT-mode/issues/1)
