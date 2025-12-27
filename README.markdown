# Agent-code

## Usage

Common Lisp implementation of coding agent. It can use open source models to help with analyzing, refactoring and implementing code.

## Installation

### Ollama(https://ollama.com/):

Use Ollama to manage open source LLM models. 

To move models from default Ollama directory to some other user defined directory:

sudo systemctl edit ollama.service

[Service]
BindPaths=/user/specified/directory:/var/lib/ollama/blobs



