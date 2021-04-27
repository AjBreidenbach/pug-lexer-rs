const fs = require('fs'), path = require('path')
let args = process.argv.slice(2)

const lex = require('pug-lexer')


TEST_CASES_DIR = path.join(__dirname, 'cases')



for(let arg of args) {
  let testCase = path.join(TEST_CASES_DIR, arg)

  //console.log({testCase, arg})

  console.log(JSON.stringify(lex(fs.readFileSync(testCase).toString(), {filename: testCase})))

}
