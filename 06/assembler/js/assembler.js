const fs = require('fs')
const parser = require('./parser.js')


const src = process.argv[2]
const dest = process.argv[3]

if(src && dest) {
    try {
        fs.readFile(src, {encoding: 'utf-8', flag: 'r'}, (err, data) => {
            if(err) throw err;

            const instructionList = parser.getInstructionList(data)
            const hack = parser.assemble(parser.firstPass(instructionList))

            fs.writeFile(dest, hack.trim(), (err) => {
                if(err) throw err;
            })
        })
    }
    catch(err) {
        console.log(err)
    }
}
else {
    console.log('Usage: node assembler.js src.asm dest.hack')
}
