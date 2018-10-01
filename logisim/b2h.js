/**
 * This is a program to translate a binary hack machine code
 * into an hexadecimal format that can be loaded into a Logisim ROM
 */

const fs = require('fs')


function b2h(b) {
    return 'v2.0 raw\n' + b.trim().split('\n').map(v => ('0000' + parseInt(v, 2).toString(16)).slice(-4)).join('\n')
}


const src = process.argv[2]
const dest = process.argv[3]

if(src && dest) {
    try {
        fs.readFile(src, {encoding: 'utf-8', flag: 'r'}, (err, data) => {
            if(err) throw err;

            const out = b2h(data)

            fs.writeFile(dest, out, (err) => {
                if(err) throw err;
            })
        })
    }
    catch(err) {
        console.log(err)
    }
}
else {
    console.log('Usage: node b2h.js src.hack dest.hex')
}

