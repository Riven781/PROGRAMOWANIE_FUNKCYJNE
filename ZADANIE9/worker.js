import { parentPort, workerData } from 'worker_threads';

//uproszcona funckja, która zwraca to co było zawarte w poleceniu (jednak nie wykorzystuje do tego map)
function mapReduce(data){
  return data.reduce((acc, x) => {
    acc[x.student] = (acc[x.student] ?? 0) + x.hours;
    return acc
  }, {})
}

const result = mapReduce(workerData);
parentPort.postMessage(result);