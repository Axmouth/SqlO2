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
    this.queryString = `CREATE TABLE regulars (id INT, name TEXT);`;
    await this.onQuerySubmit();
    this.queryString = `INSERT INTO regulars VALUES (1, '25th Baam');`;
    await this.onQuerySubmit();
    this.queryString = `INSERT INTO regulars VALUES (2, 'Rachel'); `;
    await this.onQuerySubmit();
    this.queryString = `INSERT INTO regulars VALUES (3, 'Rak WraithKaiser');`;
    await this.onQuerySubmit();
    this.queryString = `INSERT INTO regulars VALUES (4, 'Khun Aguero Agnes');`;
    await this.onQuerySubmit();
    this.queryString = `SELECT id, name FROM regulars;`;
    await this.onQuerySubmit();
    this.queryString = `SELECT id, name FROM regulars where id != 2;`;
    await this.onQuerySubmit();
    this.queryString = `SELECT id, name FROM regulars where name = 'Rachel'`;
    await this.onQuerySubmit();
    this.queryString = `SELECT id, name as charName FROM regulars where name = 'Rachel';`;
    await this.onQuerySubmit();
    this.queryString = `SELECT id, name as "regulars.charName" FROM regulars where name = 'Rachel';`;
    await this.onQuerySubmit();
  }

  async onQuerySubmit() {
    this.canType = false;
    const result = await this.postgrustqlService.eval(this.queryString);
    const newHistoryObject: ExecutedQuery = { queryString: this.queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.queryString = '';
    this.canType = true;
  }
}
