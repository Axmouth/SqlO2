import { Component, OnInit } from '@angular/core';
import { PostgrustqlService } from '../../services/postgrustql.service';

class ExecutedQuery {
  queryString: string;
  queryResults: any;
}

@Component({
  selector: 'app-repl',
  templateUrl: './repl.component.html',
  styleUrls: ['./repl.component.scss'],
})
export class ReplComponent implements OnInit {
  execHistory: ExecutedQuery[] = [];
  queryString = '';
  canType = false;

  constructor(private postgrustqlService: PostgrustqlService) {}

  async ngOnInit() {
    let queryString = `CREATE TABLE regulars (id INT, name TEXT);`;
    let result = await this.postgrustqlService.eval(queryString);
    let newHistoryObject: ExecutedQuery = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    queryString = `INSERT INTO regulars VALUES (1, '25th Baam');`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    queryString = `INSERT INTO regulars VALUES (2, 'Rachel'); `;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    queryString = `INSERT INTO regulars VALUES (3, 'Rak WraithKaiser');`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    queryString = `INSERT INTO regulars VALUES (4, 'Khun Aguero Agnes');`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.canType = true;
    queryString = `SELECT id, name FROM regulars;`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.canType = true;
    queryString = `SELECT id, name FROM regulars where id != 2;`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.canType = true;
    queryString = `SELECT id, name FROM regulars where name = 'Rachel'`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.canType = true;
    queryString = `SELECT id, name as charName FROM regulars where name = 'Rachel';`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.canType = true;
    queryString = `SELECT id, name as "regulars.charName" FROM regulars where name = 'Rachel';`;
    result = await this.postgrustqlService.eval(queryString);
    newHistoryObject = { queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.canType = true;
  }

  async onQuerySubmit() {
    this.canType = false;
    console.log(this.queryString);
    const result = await this.postgrustqlService.eval(this.queryString);
    const newHistoryObject: ExecutedQuery = { queryString: this.queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.queryString = '';
    this.canType = true;
  }
}
