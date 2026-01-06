import express from 'express';
import { Worker } from 'worker_threads';

const app = express();

app.use(express.json());

/*
3.0 zwróci wartość binarną czy podana na wejściu liczba jest liczbą
pierwszą; wykorzysta Promise
*/ 

function isPrime(num){
  return new Promise((res, rej) => {
    if (typeof num !== 'number'){
      rej('Argument is not a number')
    }
    if (num < 2){
      res(false)
    }
    for (let i = 2; i <= Math.floor(Math.sqrt(num)); i++){
      if (num % i === 0){
        res(false)
      }
    }
    res(true)
  })
}



app.post('/isPrime', (req, res) => {
  const {num} = req.body;
  isPrime(num).then(result => {
    res.json({result: result})}).catch(err => {
      res.json({error: err})
    });
});


/*
3.5  zwróci posortowaną listę; wykorzysta Promise
*/ 


function sortList(list){
  return new Promise((res, rej) => {
    if (!Array.isArray(list)){
      rej('Argument is not an array')
    }
    const sortedList = [...list].sort((a, b) => a - b)
    res(sortedList)
  })
}

app.post('/sortList', (req, res) => {
  const {list} = req.body;
  sortList(list).then(result => {
    res.json({result: result})}).catch(err => {
      res.json({error: err})
    });
});


app.listen(3000, () => {
  console.log('Server is running on port 3000');
});