const R = require('ramda')
const code = require('./code.js')
const symbol = require('./symboltable.js')


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
 * Given an instruction list and a symbol table, returns the
 * updated symbol table containing the addresses of the labels
 * declared in the instruction list
 */
const updateSymbolTable = function (instructionList, symbolTable) {
    const table = R.addIndex(R.reduce)(
        (acc, v, i) => {
            if(R.test(/\(.*\)/)(v))
                //if the instruction is a label declaration,
                //insert its name in the symbol table
                return R.assoc(R.match(/\((.*)\)/)(v)[1],
                               i - R.length(R.keys(acc)), acc)
            return acc
        },
        {},
        instructionList
    )

    //merge the resulting table with the original table
    return R.merge(table, symbolTable)
}

/**
 * Replaces each symbol in the instruction list with its
 * respective address value (both labels and variables)
 */
const resolveSymbols = function (instructionList, symbolTable) {
    return R.mapAccum (
        (acc, x) => {
            //counter to keep track of how many
            //variables were declared
            const counter = acc[0]
            //updated table with variable addresses
            const table = acc[1]

            if(R.test(/^@.*/)(x)) {
                const name = R.match(/^@(.*)/)(x)[1]

                if(isNaN(name)) { //name is a symbol
                    if(!R.has(name, table)) {
                        //if name is not in the table, add it
                        //to the table and increment counter
                        table[name] = counter
                        return [[counter + 1, table],
                                '@' + counter]
                    }
                    //if it is a symbol,
                    //replace it by its address value
                    return [[counter, table],
                            '@' + table[name]]
                }
            }
            return [[counter, table], x]
        },
        [16, symbolTable],
        instructionList
    )[1]
}

/**
 * Removes label declarations from the instruction list
 */
const removeSymbols = R.filter(R.compose(R.not, R.test(/\(.*\)/)))

/**
 * Turns an instruction list into a symbol-less instruction list
 */
const firstPass = function (instructionList) {
    const newTable = updateSymbolTable(instructionList, symbol.table)
    return removeSymbols(resolveSymbols(instructionList, newTable))
}

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
const assemble = R.pipe (
    R.map ((v) => {
        if(v[0] == '@')
            return binOfA(v)
        return binOfC(v)
    }),
    R.reduce((x,y) => R.concat(R.concat(x)('\n'))(y))('')
)


module.exports = {
    getInstructionList: getInstructionList,
    assemble: assemble,
    firstPass: firstPass
}
