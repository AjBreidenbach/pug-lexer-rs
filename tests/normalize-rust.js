
const fs = require('fs')
const dashify = require('dashify')

let input = fs.readFileSync(0).toString()

let json = JSON.parse(input)

let output = []


for(let token of json) {
  let type
  if (typeof token.lex_token !== 'string') {
    type = Object.keys(token.lex_token)[0]
    Object.assign(token, token.lex_token[type])
  } else {
    type = token.lex_token
  }
  delete token.lex_token
  // will be more here
  token.type = dashify(type)
  output.push(token)
}


console.log(JSON.stringify(output))
