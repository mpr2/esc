const R = require('ramda')
const code = require('./code.js')

const getInstructionList = R.pipe (
    R.split(/\r?\n/),
    R.map(R.trim),
    R.filter(R.pipe(R.isEmpty, R.not)),
    R.map(R.pipe(R.split('//'), R.head)),
    R.map(R.trim),
    R.filter(R.pipe(R.isEmpty, R.not))
)

const binOfA = R.pipe (
    R.split('@'),
    R.tail, R.head,
    Number,
    dec => ('000000000000000' + dec.toString(2)).slice(-16)
)

const binOfC = R.pipe (
    R.match(/(?:([^=;]*)=)?([^;]*)(?:;(.*))?/),
    R.addIndex(R.filter)((v,i) => 0 < i && i < 4),
    R.addIndex(R.map)((v,i) => code[i][v || '']),
    x => R.concat(R.reverse(R.take(2,x)), R.takeLast(1,x)),
    R.reduce(R.concat)('111')
)


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
