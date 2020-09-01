import { Component, OnInit, ViewChild, ElementRef, AfterViewInit, OnDestroy } from '@angular/core';
import { PostgrustqlService } from '../../services/postgrustql.service';
import { ActivatedRoute, Router, NavigationEnd } from '@angular/router';
import { Subject } from 'rxjs';
import { take, takeUntil } from 'rxjs/operators';

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
  @ViewChild('promptInput') promptInputRef: ElementRef;

  constructor(private postgrustqlService: PostgrustqlService, private route: ActivatedRoute, private router: Router) {}

  ngOnInit() {
    this.router.events
      .pipe(take(1))
      .pipe(takeUntil(this.notifier))
      .subscribe(async (event) => {
        if (event instanceof NavigationEnd) {
          // Get a good value
          const params = this.route.snapshot.queryParams;
          console.log(params);

          if (Array.isArray(params.q)) {
            for (const query of params.q) {
              this.queryString = query;
              await this.onQuerySubmit();
            }
          } else if (params.q === '') {
            return;
          } else if (params.q !== undefined) {
            this.queryString = params.q;
            await this.onQuerySubmit();
          } else {
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
        }
      });
  }

  async onQuerySubmit() {
    this.canType = false;
    const result = await this.postgrustqlService.eval(this.queryString);
    const newHistoryObject: ExecutedQuery = { queryString: this.queryString, queryResults: result };
    this.execHistory.push(newHistoryObject);
    this.promptInputRef.nativeElement.scrollIntoView();
    this.promptInputRef.nativeElement.focus();
    this.queryString = '';
    this.canType = true;
  }

  focusInput() {
    this.promptInputRef.nativeElement.focus();
  }

  ngOnDestroy() {
    this.notifier.next();
    this.notifier.complete();
  }
}
