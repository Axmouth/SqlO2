import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { ReplComponent } from './components/repl/repl.component';
import { ResultsTableComponent } from './components/results-table/results-table.component';
import { A11yModule } from '@angular/cdk/a11y';

@NgModule({
  declarations: [AppComponent, ReplComponent, ResultsTableComponent],
  imports: [BrowserModule, AppRoutingModule, FormsModule, A11yModule],
  providers: [],
  bootstrap: [AppComponent],
})
export class AppModule {}
