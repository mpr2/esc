const R = require('ramda')
const code = require('./code.js')

/**
 * Accepts a source code string and returns
 * a list of instructions to be parsed
 */
const getInstructionList = R.pipe (
    R.split(/\r?\n/),
    R.map(R.pipe(R.split('//'), R.head)),
    R.map(R.trim),
    R.filter(R.pipe(R.isEmpty, R.not))
)

/**
 * Returns the binary code of a symbol-less A instruction
 */
const binOfA = R.pipe (
    R.replace('@')(''),
    Number,
    dec => ('000000000000000' + dec.toString(2)).slice(-16)
)

/**
 * Returns the binary code of a C instruction
 */
const binOfC = R.pipe (
    //capture each part of the instruction
    R.match(/(?:([^=;]*)=)?([^;]*)(?:;(.*))?/),
    //keep only the captured groups
    R.addIndex(R.filter)((v,i) => 0 < i && i < 4),
    //lookup the code for each part
    R.addIndex(R.map)((v,i) => code[i][v || '']),
    //swap dest with comp
    x => R.concat(R.reverse(R.take(2,x)), R.takeLast(1,x)),
    R.reduce(R.concat)('111')
)

/**
 * Takes a list of symbol-less instructions and
 * gives a string containing the assembled machine code
 */
const assemble = R.pipe(
    R.map ((v) => {
        if(v[0] == '@')
            return binOfA(v)
        return binOfC(v)
    }),
    R.reduce((x,y) => R.concat(R.concat(x)('\n'))(y))('')
)


module.exports = {
    getInstructionList: getInstructionList,
    assemble: assemble
}
