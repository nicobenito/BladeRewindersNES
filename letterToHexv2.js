const letters = {
    a: "0A",
    b: "0B",
    c: "0C",
    d: "0D",
    e: "0E",
    f: "0F",
    g: "10",
    h: "11",
    i: "12",
    j: "13",
    k: "14",
    l: "15",
    m: "16",
    n: "17",
    o: "18",
    p: "19",
    q: "1A",
    r: "1B",
    s: "1C",
    t: "1D",
    u: "1E",
    v: "1F",
    w: "20",
    x: "21",
    y: "22",
    z: "23",
    space: "24",
    0: "00",
    1: "01",
    2: "02",
    3: "03",
    4: "04",
    5: "05",
    6: "06",
    7: "07",
    8: "08",
    9: "09"
};

// const message = "blade rewinders message builder first test";
const message = `
en la decada de los 90 el vhs era sinonimo de entretenimiento
debido a la imparable demanda ningun videoclub tenia tiempo suficiente de rebobinar sus peliculas`;

const stringToHex = () => {
    let stringHex = '';
    let counter = 30;
    for (let i = 0; i < message.length; i++) {
        // stringHex += '$'
        if(counter == 0) {
            stringHex += '$'
            stringHex += letters.space;
            stringHex += ', '
            stringHex += '$'
            stringHex += letters.space;
            stringHex += ', '
            counter = 30
        }
        
        if (!message[i].replace(/\s/g, '').length) {
            stringHex += '$'
            stringHex += letters.space;
        }
        else if (message[i] == '\n') {
            for (let i = 0; i < counter; i++) {
                stringHex += '$'
                stringHex += letters.space; 
                stringHex += ', '
            }   
            counter = 30         
        }
        else {
            stringHex += '$'
            stringHex += letters[message[i]];
        }
            
        stringHex += ', '
        counter -= 1;
    }
    console.log(stringHex);
    console.log(message.length);
    console.log(stringHex.split(", ").length.toString(16));
}

stringToHex();