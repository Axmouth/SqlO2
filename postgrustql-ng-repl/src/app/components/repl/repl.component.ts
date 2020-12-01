import { Component, OnInit, ViewChild, ElementRef, OnDestroy } from '@angular/core';
import { PostgrustqlService } from '../../services/postgrustql.service';
import { ActivatedRoute, Router, NavigationEnd } from '@angular/router';
import { Subject } from 'rxjs';
import { take, takeUntil } from 'rxjs/operators';
import { CdkTextareaAutosize } from '@angular/cdk/text-field';

class ExecutedQuery {
  queryString: string;
  queryResults: any;
}

@Component({
  selector: 'app-repl',
  templateUrl: './repl.component.html',
  styleUrls: ['./repl.component.scss'],
})
export class ReplComponent implements OnInit, OnDestroy {
  notifier = new Subject();
  execHistory: ExecutedQuery[] = [];
  queryString = '';
  canType = false;
  @ViewChild('promptInputRef')
  promptInputRef: ElementRef;
  @ViewChild('promptInput')
  contentFCAutosize: CdkTextareaAutosize;
  historyPosition = 0;
  current = '';

  constructor(private postgrustqlService: PostgrustqlService, private route: ActivatedRoute, private router: Router) {}

  ngOnInit() {
    this.router.events.pipe(takeUntil(this.notifier)).subscribe(async (event) => {
      if (event instanceof NavigationEnd) {
        // Get a good value
        const params = this.route.snapshot.queryParams;
        console.log(params);

        if (Array.isArray(params.q)) {
          for (const query of params.q) {
            this.queryString = query;
            await this.submitQuery();
          }
        } else if (params.q === '') {
          return;
        } else if (params.q !== undefined) {
          this.queryString = params.q;
          await this.submitQuery();
        } else {
          this.queryString = `CREATE TABLE characters (id INT PRIMARY KEY, name TEXT);`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (0, 'Phantaminum');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (1, 'The 25th Bam');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (2, 'Rachel'); `;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (3, 'Rak WraithKaiser');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (4, 'Khun Aguero Agnes');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (5, 'King Zahard');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (6, 'Ha Yuri Zahard');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (7, 'Androssi Zahard');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (7, 'Evankhell');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (8, 'Evankhell');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (9, 'Anak Zahard');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (10, 'Yeon Yihwa');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (11, 'Yeo Miseng');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (12, 'Yeo Goseng');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (13, 'Xia Xia');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (14, 'Sachi Faker');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (15, 'Hwa Ryun');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (16, 'Khun Ran');`;
          await this.submitQuery();
          this.queryString = `INSERT INTO characters VALUES (17, 'Ha Yura');`;
          await this.submitQuery();
          this.queryString = `SELECT id, name FROM characters;`;
          await this.submitQuery();
          this.queryString = `SELECT id, name FROM characters WHERE id != 2;`;
          await this.submitQuery();
          this.queryString = `SELECT id, name FROM characters WHERE name = 'Rachel'`;
          await this.submitQuery();
          this.queryString = `SELECT id, name as charName FROM characters WHERE name != 'Rachel' AND id < 5;`;
          await this.submitQuery();
          this.queryString = `SELECT name FROM characters ORDER BY name ASC`;
          await this.submitQuery();
          this.queryString = `SELECT DISTINCT (id / 2)::int FROM characters`;
          await this.submitQuery();
          // tslint:disable-next-line:max-line-length
          // this.queryString = `SELECT id::text || ' ' || name AS name_with_id FROM characters WHERE id > 1 ORDER BY id DESC LIMIT 4 OFFSET 5;`;
          // await this.submitQuery();
          this.queryString = `CREATE TABLE character_roles (character_id INT, role_name TEXT); INSERT INTO character_roles VALUES (1, 'Wave Controller'); INSERT INTO character_roles VALUES (2, 'Light Bearer'); INSERT INTO character_roles VALUES (3, 'Spear Bearer'); INSERT INTO character_roles VALUES (4, 'Light Bearer'); INSERT INTO character_roles VALUES (1, 'Fisherman'); INSERT INTO character_roles VALUES (4, 'Spear Bearer');`;
          await this.submitQuery();
          // tslint:disable-next-line:max-line-length
          this.queryString = `SELECT * FROM characters INNER JOIN character_roles ON characters.id=character_roles.character_id WHERE id != 2 ORDER BY id;`;
          await this.submitQuery();
        }
      }
    });
  }

  async submitQuery() {
    this.canType = false;
    const queryString = this.queryString;
    const queryResults = await this.postgrustqlService.eval(queryString.trim());
    const newHistoryObject: ExecutedQuery = { queryString, queryResults };
    this.execHistory.push(newHistoryObject);
    this.promptInputRef.nativeElement.scrollIntoView();
    this.promptInputRef.nativeElement.focus();
    this.queryString = '';
    this.resetTextArea();
    this.historyPosition = this.execHistory.length;
    this.current = '';
    this.canType = true;
  }

  async onQuerySubmit(event: Event) {
    event.preventDefault();
    await this.submitQuery();
  }

  onHistoryUp(event: Event) {
    event.preventDefault();
    if (this.historyPosition <= 0) {
      return;
    }
    this.historyPosition--;
    if (this.historyPosition === this.execHistory.length - 1) {
      this.current = this.queryString;
    }
    this.queryString = this.execHistory[this.historyPosition].queryString;
  }

  onHistoryDown(event: Event) {
    event.preventDefault();
    if (this.historyPosition === this.execHistory.length - 1) {
      this.queryString = this.current;
      this.historyPosition++;
      return;
    } else if (this.historyPosition > this.execHistory.length - 1) {
      this.queryString = this.current;
      return;
    }
    this.historyPosition++;
    this.queryString = this.execHistory[this.historyPosition].queryString;
  }

  focusInput() {
    this.promptInputRef.nativeElement.focus();
  }

  ngOnDestroy() {
    this.notifier.next();
    this.notifier.complete();
  }

  public resetTextArea() {
    this.contentFCAutosize.reset();
  }

  public autoGrow(event: Event) {
    const textArea = this.promptInputRef.nativeElement;
    textArea.style.overflow = 'hidden';
    textArea.style.height = textArea.scrollHeight + 'px';
    this.promptInputRef.nativeElement.scrollIntoView();
  }
}
