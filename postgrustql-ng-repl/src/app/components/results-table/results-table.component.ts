import { Component, OnInit, Input } from '@angular/core';

@Component({
  selector: 'app-results-table',
  templateUrl: './results-table.component.html',
  styleUrls: ['./results-table.component.scss'],
})
export class ResultsTableComponent implements OnInit {
  @Input()
  results: { rows: any[][]; columns: string[] };

  constructor() {}

  ngOnInit(): void {}
}
