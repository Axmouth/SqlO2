import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { ReplComponent } from './components/repl/repl.component';
import { ResultsTableComponent } from './components/results-table/results-table.component';

@NgModule({
  declarations: [AppComponent, ReplComponent, ResultsTableComponent],
  imports: [BrowserModule, AppRoutingModule, FormsModule],
  providers: [],
  bootstrap: [AppComponent],
})
export class AppModule {}
